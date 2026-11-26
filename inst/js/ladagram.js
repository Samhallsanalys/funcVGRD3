/* vgr_D3 — lådagram (func_d3_ladagram).
 *
 * Statistiken per låda (kvartiler, morrhår, medelvärde, avvikande värden) kommer från R och räknas på samma sätt
 * som func_graf_ladagram. Här ritas lådorna, stående eller liggande, med grupper bredvid varandra inom varje
 * kategori. Hela SVG:en byggs om vid varje ändring; vid byte av filtervärde glider lådorna från sina tidigare
 * värden till de nya.
 */
(function () {
  "use strict";
  var vgrD3 = window.vgrD3;
  function pt(v) { return v * 96 / 72; }
  var LINES = pt(11 * 1.2);
  var SEP = "\u0001";

  vgrD3.renderers.ladagram = function (el, spec) {
    var W = spec.width, H = spec.height, font = spec.font;
    var T = spec.text, L = spec.legend, LB = spec.labels, TT = spec.tooltip;
    var IA = spec.interactive, F = spec.facets, BX = spec.box, PTS = spec.points, HOR = spec.horizontal;
    var dur = spec.animation.duration || 0;
    var esc = vgrD3.esc, fmt = vgrD3.fmt;
    var fileName = vgrD3.slug((T.title && T.title.join(" ")) || "ladagram");

    var root = d3.select(el).style("max-width", W + "px");
    var controls = vgrD3.controls(el, font);
    var svg = root.append("svg").attr("class", "vgr-d3-svg")
      .attr("width", W).attr("height", H).attr("viewBox", "0 0 " + W + " " + H)
      .attr("role", "img").attr("aria-label", (T.title && T.title.join(" ")) || "Lådagram")
      .attr("font-family", font)
      .style("width", "100%").style("height", "auto").style("display", "block");
    var tip = vgrD3.tooltip(el, font);

    if (spec.empty) {
      svg.append("text").attr("x", W / 2).attr("y", H / 2).attr("text-anchor", "middle")
        .style("font-size", (18 * (spec.text_scale || 1)) + "px").text("Inga data att visa");
      return;
    }

    var groups = spec.has_groups ? spec.groups : [{ key: "__alla", label: "", color: BX.color }];
    var groupByKey = {}, groupIndex = {};
    groups.forEach(function (g, i) { groupByKey[g.key] = g; groupIndex[g.key] = i; });
    // Lådorna per filtervärde och panel, så att varje ritning bara går igenom sin del
    var boxesByKF = d3.group(spec.boxes, function (b) { return b.k + SEP + b.f; });
    var hasFacets = !(F.levels.length === 1 && F.levels[0] === "__alla");
    var freeVal = F.free_value, freeCat = F.free_category;
    var catSize = T.x_axis_text_size, valSize = T.y_axis_text_size;
    // Kategorinamnens bredd ändras inte mellan ritningarna: mät en gång
    var catW = spec.has_categories ? d3.max(spec.categories, function (c) { return vgrD3.measureText(svg, c, catSize, font); }) || 0 : 0;

    // prevStats: lådornas värden i förra ritningen (nyckel -> statistik), för övergången vid filterbyte
    var state = { filter: spec.filter.default, hidden: {}, sort: "original", prevStats: null, first: true };

    function boxKey(b) { return b.f + SEP + b.x + SEP + b.g; }
    // Frö i [0, 1) ur lådans nyckel: punkterna sprids likadant varje gång, även när andra lådor döljs
    function seedOf(key) {
      var h = 0;
      for (var i = 0; i < key.length; i++) h = (h * 31 + key.charCodeAt(i)) % 1000003;
      return h / 1000003;
    }
    function boxesIn(k, f) {
      return (boxesByKF.get(k + SEP + f) || []).filter(function (b) { return !state.hidden[b.g]; });
    }
    // Kategoriordning: som i R, efter medianen av alla värden i kategorin (samma regel som sort_by; nyckeln
    // räknas i R per filtervärde och panel) eller efter namnet
    function categoriesFor(f) {
      var shown = boxesIn(state.filter, f);
      var present = {};
      shown.forEach(function (b) { present[b.x] = true; });
      var cats = spec.categories.filter(function (c) { return !freeCat || present[c]; });
      if (state.sort === "name_asc") return cats.slice().sort(function (a, b) { return a.localeCompare(b, "sv"); });
      if (state.sort === "name_desc") return cats.slice().sort(function (a, b) { return b.localeCompare(a, "sv"); });
      if (state.sort === "value_asc" || state.sort === "value_desc") {
        var med = (spec.cat_median && spec.cat_median[state.filter] && spec.cat_median[state.filter][f]) || {};
        var sign = state.sort === "value_desc" ? -1 : 1;
        return cats.slice().sort(function (a, b) {
          if (med[a] == null) return med[b] == null ? 0 : 1;
          if (med[b] == null) return -1;
          return sign * (med[a] - med[b]);
        });
      }
      return cats;
    }

    function refsOf() {
      var v = spec.ref.y ? spec.ref.y[state.filter] : null;
      return v == null ? [] : [].concat(v);
    }
    // vmin/vmax per låda kommer från R: det som värdeaxeln måste visa (morrhår och de punkter som ritas)
    function valueDomainFor(facets) {
      var keys = spec.filter.same_y ? spec.filter.values : [state.filter];
      var lo = Infinity, hi = -Infinity;
      keys.forEach(function (k) {
        facets.forEach(function (f) {
          boxesIn(k, f).forEach(function (b) { lo = Math.min(lo, b.vmin); hi = Math.max(hi, b.vmax); });
        });
      });
      refsOf().forEach(function (v) { lo = Math.min(lo, v); hi = Math.max(hi, v); });
      if (!isFinite(lo)) { lo = 0; hi = 1; }
      // y_axis_min/max ersätter ändarna, och axeln får 5 % luft utanför som i func_graf_ladagram
      if (spec.y.min != null) lo = spec.y.min;
      if (spec.y.max != null) hi = spec.y.max;
      if (lo === hi) { lo -= Math.abs(lo) * 0.1 || 1; hi += Math.abs(hi) * 0.1 || 1; }
      var pad = (hi - lo) * 0.05;
      return [lo - pad, hi + pad];
    }

    // -------------------------------------------------------------------------
    //  Förklaring (klickbar)
    // -------------------------------------------------------------------------
    var legendItems = groups.map(function (g) { return { g: g, w: pt(12) + 6 + vgrD3.measureText(svg, g.label, L.text_size, font) }; });
    var legendTitleW = L.title ? vgrD3.measureText(svg, L.title, L.title_size, font) : 0;
    function drawLegend(area) {
      if (!spec.has_groups || L.position === "none") return;
      var rowH = Math.max(L.text_size * 1.3, pt(17.28)), sw = pt(12);
      var vertical = L.position === "left" || L.position === "right";
      var items = legendItems;
      var titleH = L.title && vertical ? L.title_size * 1.6 : 0;
      var lw, lh;
      if (vertical) {
        items.forEach(function (it, i) { it.x = 0; it.y = titleH + i * rowH; });
        lw = Math.max(legendTitleW, d3.max(items, function (it) { return it.w; })) + 8;
        lh = titleH + items.length * rowH;
      } else {
        var startX = L.title ? legendTitleW + 10 : 0, avail = area.x1 - area.x0, x = startX, y = 0, maxX = 0;
        items.forEach(function (it) {
          if (x > startX && x + it.w > avail) { x = startX; y += rowH; }
          it.x = x; it.y = y; x += it.w + 16; maxX = Math.max(maxX, x - 16);
        });
        lw = maxX; lh = y + rowH;
      }
      var lx, ly, gap = pt(11);
      if (L.position === "right") { lx = area.x1 - lw; ly = (area.y0 + area.y1) / 2 - lh / 2; area.x1 -= lw + gap; }
      else if (L.position === "left") { lx = area.x0; ly = (area.y0 + area.y1) / 2 - lh / 2; area.x0 += lw + gap; }
      else if (L.position === "top") { lx = (area.x0 + area.x1) / 2 - lw / 2; ly = area.y0; area.y0 += lh + gap; }
      else { lx = (area.x0 + area.x1) / 2 - lw / 2; ly = area.y1 - lh; area.y1 -= lh + gap; }
      var lg = svg.append("g").attr("transform", "translate(" + lx + "," + ly + ")");
      if (L.title) {
        lg.append("text").attr("y", vertical ? L.title_size : rowH / 2).attr("dy", vertical ? null : "0.35em")
          .style("font-size", L.title_size + "px").style("font-family", font).text(L.title);
      }
      var item = lg.selectAll("g.vgr-leg").data(items).join("g").attr("class", "vgr-leg")
        .attr("transform", function (it) { return "translate(" + it.x + "," + it.y + ")"; })
        .attr("opacity", function (it) { return state.hidden[it.g.key] ? 0.35 : 1; })
        .style("cursor", IA.legend ? "pointer" : null);
      item.append("rect").attr("width", function (it) { return it.w; }).attr("height", rowH).attr("fill", "#ffffff").attr("fill-opacity", 0);
      item.append("rect").attr("y", (rowH - sw) / 2).attr("width", sw).attr("height", sw)
        .attr("fill", function (it) { return it.g.color; }).attr("fill-opacity", BX.alpha)
        .attr("stroke", BX.line_color).attr("stroke-width", BX.line_width);
      item.append("text").attr("x", sw + 6).attr("y", rowH / 2).attr("dy", "0.35em")
        .style("font-size", L.text_size + "px").style("font-family", font).text(function (it) { return it.g.label; });
      if (IA.legend) {
        item.append("title").text("Klicka: dölj/visa · Dubbelklicka: visa bara denna");
        item.on("click", function (event, it) {
          if (state.hidden[it.g.key]) delete state.hidden[it.g.key]; else state.hidden[it.g.key] = true;
          draw();
        }).on("dblclick", function (event, it) {
          state.hidden = {};
          groups.forEach(function (g) { if (g.key !== it.g.key) state.hidden[g.key] = true; });
          draw();
        });
      }
    }

    // -------------------------------------------------------------------------
    //  Rita allt
    // -------------------------------------------------------------------------
    var newStats = {};
    function draw(animateFrom) {
      svg.selectAll("*").interrupt();
      svg.selectAll("*").remove();
      tip.hide();
      newStats = {};
      svg.append("rect").attr("width", W).attr("height", H).attr("fill", "#ffffff");
      var m = { t: pt(hasFacets && F.wrapped ? 20 : 15), r: pt(10), b: pt(10), l: pt(10) };
      var box = { x0: m.l, y0: m.t, x1: W - m.r, y1: H - m.b };
      var y = box.y0;
      var th = vgrD3.textBlock(svg, T.title, { x: box.x0, y: y, size: T.title_size, font: font, anchor: "start", lineHeight: 1.1 });
      y += th + (th ? pt(5) : 0);
      var sh = vgrD3.textBlock(svg, T.subtitle, { x: box.x0, y: y, size: T.subtitle_size, font: font, anchor: "start", lineHeight: 1.2 });
      y += sh + (sh ? pt(5) : 0);
      if (th || sh) y += pt(5);
      var bottom = box.y1;
      var ch = T.caption.length * T.caption_size * 1.2;
      if (ch) bottom -= ch + pt(5.5);
      var area = { x0: box.x0, y0: y, x1: box.x1, y1: bottom };
      drawLegend(area);
      // Axeltitlar: x_axis_title hör till kategoriaxeln, y_axis_title till värdeaxeln (som coord_flip i R)
      var bottomTitle = HOR ? T.y_axis_title : T.x_axis_title, leftTitle = HOR ? T.x_axis_title : T.y_axis_title;
      if (bottomTitle) {
        svg.append("text").attr("text-anchor", "middle")
          .attr("transform", "translate(" + ((area.x0 + area.x1) / 2) + "," + (area.y1 - T.axis_title_size * 0.25) + ")")
          .style("font-size", T.axis_title_size + "px").style("font-family", font).text(bottomTitle);
        area.y1 -= T.axis_title_size * 1.5;
      }
      if (leftTitle) {
        svg.append("text").attr("text-anchor", "middle")
          .attr("transform", "translate(" + (area.x0 + T.axis_title_size * 0.9) + "," + ((area.y0 + area.y1) / 2) + ") rotate(-90)")
          .style("font-size", T.axis_title_size + "px").style("font-family", font).text(leftTitle);
        area.x0 += T.axis_title_size * 1.6;
      }
      var panelRight = layoutPanels(area, animateFrom);
      if (ch) vgrD3.textBlock(svg, T.caption, { x: panelRight, y: box.y1 - ch, size: T.caption_size, font: font, anchor: "end" });
      state.prevStats = newStats;
      refreshControls();
      state.first = false;
    }

    // Värdeaxelns markeringar
    function valTicks(scale) {
      var t = scale.ticks(5);
      var dec = t.length > 1 ? d3.precisionFixed(t[1] - t[0]) : 0;
      return t.map(function (v) { return { v: v, label: fmt(v, dec) }; });
    }

    function layoutPanels(area, animateFrom) {
      var n = F.levels.length;
      var ncol = Math.min(F.ncol, n), nrow = Math.ceil(n / ncol);
      var sx = hasFacets ? (F.wrapped ? 0.3 : 0.6) * LINES : 0;
      var sy = hasFacets ? (F.wrapped ? 0.2 : 0.8) * LINES : 0;
      var stripLines = d3.max(F.levels, function (f) { return String(f).split("\n").length; });
      var stripH = hasFacets ? stripLines * T.facet_title_size * 0.9 + pt(F.wrapped ? 14 : 16) : 0;
      var shared = freeVal ? null : valueDomainFor(F.levels);
      var doms = F.levels.map(function (f) { return shared || valueDomainFor([f]); });

      // Värdenas bredd (en gång när alla paneler har samma värdeaxel)
      var valW = 0;
      (shared ? [shared] : doms).forEach(function (dom) {
        valTicks(d3.scaleLinear().domain(dom)).forEach(function (t) { valW = Math.max(valW, vgrD3.measureText(svg, t.label, valSize, font)); });
      });
      var rad = spec.x_angle * Math.PI / 180;
      // Liggande: kategorinamnen till vänster, roterade med x_angle tar de mindre bredd
      var leftW = (HOR ? (spec.x_angle > 0 ? catW * Math.cos(rad) + catSize * Math.sin(rad) : catW) : valW) + pt(4);
      var bottomH = HOR ? valSize + pt(5)
        : (spec.has_categories ? (spec.x_angle > 0 ? catW * Math.sin(rad) + catSize * Math.cos(rad) : catSize) + pt(5) : pt(3));
      var cellW = (area.x1 - area.x0 - (ncol - 1) * sx) / ncol;
      var cellH = (area.y1 - area.y0 - (nrow - 1) * sy) / nrow;
      var leftShared = hasFacets && ncol > 1 && (HOR ? !freeCat : !freeVal);
      var panelW = (area.x1 - area.x0 - leftW - (ncol - 1) * sx) / ncol;
      F.levels.forEach(function (f, i) {
        var col = i % ncol, row = Math.floor(i / ncol);
        var cx = area.x0 + col * (cellW + sx), cy = area.y0 + row * (cellH + sy);
        var rect = { x0: cx + leftW, y0: cy + stripH, x1: cx + cellW, y1: cy + cellH - bottomH };
        if (leftShared) { rect.x0 = area.x0 + leftW + col * (panelW + sx); rect.x1 = rect.x0 + panelW; }
        if (hasFacets) {
          vgrD3.textBlock(svg, String(f).split("\n"), {
            x: (rect.x0 + rect.x1) / 2, y: cy + pt(F.wrapped ? 6 : 4), size: T.facet_title_size, font: font, lineHeight: 0.9
          });
        }
        drawPanel(f, i, rect, doms[i], !leftShared || col === 0, animateFrom);
      });
      return area.x0 + ncol * cellW + (ncol - 1) * sx;
    }

    function drawPanel(facet, idx, rect, valDom, leftLabels, animateFrom) {
      var cats = categoriesFor(facet);
      // Kategoriaxeln: stående = x från vänster, liggande = y uppifrån (första kategorin överst, som i R)
      var band = d3.scaleBand().domain(cats).range(HOR ? [rect.y0, rect.y1] : [rect.x0, rect.x1]);
      var vs = d3.scaleLinear().domain(valDom).range(HOR ? [rect.x0, rect.x1] : [rect.y1, rect.y0]);
      var step = band.bandwidth();
      var nG = spec.has_groups ? groups.length : 1;
      var bw = step * BX.width * (spec.has_groups ? 0.8 / nG : 1);
      // Lådans mitt i kategoriaxelns led: grupperna bredvid varandra (alla grupper har sin plats, även dolda)
      function centre(b) {
        var c = band(b.x) + step / 2;
        return spec.has_groups ? c + (groupIndex[b.g] - (nG - 1) / 2) * 0.8 * step / nG : c;
      }
      // Punkt (kategoriled, värde) -> [x, y] och rektangel (kategoriled c0–c1, värde v0–v1) -> {x, y, w, h}:
      // de enda ställena som vet om lådorna står eller ligger
      function P(c, v) { return HOR ? [vs(v), c] : [c, vs(v)]; }
      function R(c0, c1, v0, v1) {
        var a = P(c0, v0), z = P(c1, v1);
        return { x: Math.min(a[0], z[0]), y: Math.min(a[1], z[1]), w: Math.abs(z[0] - a[0]), h: Math.abs(z[1] - a[1]) };
      }

      var g = svg.append("g").attr("class", "vgr-panel");
      var clipId = el.id + "-clip-" + idx;
      g.append("clipPath").attr("id", clipId).append("rect")
        .attr("x", rect.x0).attr("y", rect.y0).attr("width", rect.x1 - rect.x0).attr("height", rect.y1 - rect.y0);
      var clipped = g.append("g").attr("clip-path", "url(#" + clipId + ")");
      var vt = valTicks(vs);
      var catEnds = HOR ? [rect.y0, rect.y1] : [rect.x0, rect.x1];
      function valLine(sel) {
        return sel.each(function (v) {
          var a = P(catEnds[0], v), z = P(catEnds[1], v);
          d3.select(this).attr("x1", a[0]).attr("y1", a[1]).attr("x2", z[0]).attr("y2", z[1]);
        });
      }

      // Rutnät (true/false eller "x"/"y" som i R: "x" = lodräta linjer). Värdelinjer går från värdeaxeln (x när
      // lådorna ligger), kategorilinjer genom varje kategori (bara stora linjer, som en diskret axel i ggplot2).
      var grid = clipped.append("g").attr("pointer-events", "none");
      function gridOn(setting, valueAxis) { return setting === true || setting === ((valueAxis === HOR) ? "x" : "y"); }
      var tv = vt.map(function (t) { return t.v; });
      var mids = []; for (var mi = 1; mi < tv.length; mi++) mids.push((tv[mi - 1] + tv[mi]) / 2);
      var GR = spec.grid;   // färg och tjocklek: grid_* (px)
      if (gridOn(GR.minor, true)) valLine(grid.selectAll(null).data(mids).enter().append("line")).attr("stroke", GR.minor_color).attr("stroke-width", GR.minor_width);
      if (gridOn(GR.major, true)) valLine(grid.selectAll(null).data(tv).enter().append("line")).attr("stroke", GR.major_color).attr("stroke-width", GR.major_width);
      if (spec.has_categories && gridOn(spec.grid.major, false)) {
        grid.selectAll(null).data(cats).enter().append("line").each(function (c) {
          var a = P(band(c) + step / 2, valDom[0]), z = P(band(c) + step / 2, valDom[1]);
          d3.select(this).attr("x1", a[0]).attr("y1", a[1]).attr("x2", z[0]).attr("y2", z[1]);
        }).attr("stroke", GR.major_color).attr("stroke-width", GR.major_width);
      }
      if (hasFacets) {
        g.append("rect").attr("x", rect.x0).attr("y", rect.y0).attr("width", rect.x1 - rect.x0)
          .attr("height", rect.y1 - rect.y0).attr("fill", "none").attr("stroke", "#b3b3b3").attr("stroke-width", 0.85);
      }

      // Referenslinjer på värdeaxeln, en eller flera, med valfri text
      var refLabels = spec.ref.y_label == null ? [] : [].concat(spec.ref.y_label);
      refsOf().forEach(function (v, i) {
        valLine(clipped.append("line").datum(v)).attr("stroke", "#000000").attr("stroke-width", 1.42).attr("pointer-events", "none");
        var lab = refLabels.length ? refLabels[i % refLabels.length] : null;
        if (lab) {
          clipped.append("text").attr("x", HOR ? vs(v) + 5 : rect.x1 - 4).attr("y", HOR ? rect.y0 + 14 : vs(v) - 5)
            .attr("text-anchor", HOR ? "start" : "end")
            .attr("fill", "#37474F").attr("stroke", "#ffffff").attr("stroke-width", 3).attr("paint-order", "stroke")
            .style("font-size", (13 * (spec.text_scale || 1)) + "px").style("font-family", font).text(lab);
        }
      });

      // Lådor: morrhår, lådan, medianstrecket, medelvärdet, punkterna och etiketten
      var inCats = {};
      cats.forEach(function (c) { inCats[c] = true; });
      var boxes = boxesIn(state.filter, facet).filter(function (b) { return inCats[b.x]; });
      var animate = dur > 0 && ((state.first && spec.animation.grow) || !!animateFrom);
      function tr(sel) { return animate ? sel.transition().duration(dur).ease(d3.easeCubicInOut) : sel; }
      // Det som inte glider tonas in när lådorna är på plats
      function fadeIn(sel) {
        if (animate) sel.attr("opacity", 0).transition().delay(dur * 0.8).duration(200).attr("opacity", 1);
      }
      var boxG = clipped.append("g");
      boxes.forEach(function (b, bi) {
        var key = boxKey(b), c = centre(b), color = groupByKey[b.g].color;
        newStats[key] = b;
        var bg = boxG.append("g").attr("class", "vgr-box");
        // Morrhåren som en bana i två delar (nedre morrhår–Q1 och Q3–övre morrhår), så att de inte syns genom lådan
        function whiskerAttrs(sel, s) {
          return sel.attr("d", "M" + P(c, s.lower) + "L" + P(c, s.q1) + "M" + P(c, s.q3) + "L" + P(c, s.upper));
        }
        function rectAttrs(sel, s) {
          var r = R(c - bw / 2, c + bw / 2, s.q1, s.q3);
          return sel.attr("x", r.x).attr("y", r.y).attr("width", r.w).attr("height", r.h);
        }
        function medianAttrs(sel, s) {
          var a = P(c - bw / 2, s.median), z = P(c + bw / 2, s.median);
          return sel.attr("x1", a[0]).attr("y1", a[1]).attr("x2", z[0]).attr("y2", z[1]);
        }
        var whisker = bg.append("path").attr("fill", "none").attr("stroke", BX.line_color).attr("stroke-width", BX.line_width);
        var rectEl = bg.append("rect").attr("fill", color).attr("fill-opacity", BX.alpha)
          .attr("stroke", BX.line_color).attr("stroke-width", BX.line_width);
        var med = bg.append("line").attr("stroke", BX.line_color).attr("stroke-width", BX.line_width * 2.2);
        if (animate) {
          // Startläge: förra värdena (filterbyte) eller allt samlat i medianen (första ritningen)
          var from = animateFrom && animateFrom[key] ? animateFrom[key]
            : { q1: b.median, median: b.median, q3: b.median, lower: b.median, upper: b.median };
          whiskerAttrs(whisker, from); rectAttrs(rectEl, from); medianAttrs(med, from);
        }
        whiskerAttrs(tr(whisker), b); rectAttrs(tr(rectEl), b); medianAttrs(tr(med), b);
        if (BX.mean) {
          var dsz = 5.5;   // som shape = 23, size = 2.5 i func_graf_ladagram
          fadeIn(bg.append("path").attr("d", "M0," + (-dsz) + "L" + dsz + ",0L0," + dsz + "L" + (-dsz) + ",0Z")
            .attr("transform", "translate(" + P(c, b.mean) + ")")
            .attr("fill", "#ffffff").attr("stroke", BX.line_color).attr("stroke-width", 1));
        }
        // Punkter från R: alla värden spridda i sidled (point_show) eller de avvikande på lådans mittlinje.
        // Spridningen är densamma vid varje ritning (fast frö per låda).
        if (b.points.length) {
          var spread = PTS.show ? PTS.jitter * bw : 0, rnd = d3.randomLcg(seedOf(key));
          var xy = b.points.map(function (v) { return P(c + (rnd() - 0.5) * spread, v); });
          fadeIn(bg.selectAll(null).data(xy).enter().append("circle")
            .attr("cx", function (d) { return d[0]; }).attr("cy", function (d) { return d[1]; })
            .attr("r", PTS.r)
            .attr("fill", PTS.show ? "#333333" : BX.line_color).attr("fill-opacity", PTS.show ? PTS.alpha : 1)
            .attr("pointer-events", "none"));
        }
        // Medianetikett bredvid lådan (ovanför när den ligger), som i R
        if (LB.show) {
          var lp = HOR ? [vs(b.median), c - bw / 2 - 3] : [c + bw / 2 + 3, vs(b.median)];
          fadeIn(bg.append("text").attr("x", lp[0]).attr("y", lp[1]).attr("dy", HOR ? null : "0.35em")
            .attr("text-anchor", HOR ? "middle" : "start").attr("fill", LB.color)
            .style("font-size", LB.size + "px").style("font-family", font).attr("pointer-events", "none")
            .text(fmt(b.median, LB.decimals)));
        }
        // Tooltip: hela lådans område, från morrhår till morrhår
        var hz = R(c - bw / 2, c + bw / 2, b.lower, b.upper);
        bg.append("rect").attr("x", hz.x).attr("y", hz.y).attr("width", Math.max(hz.w, 1)).attr("height", Math.max(hz.h, 1))
          .attr("fill", "#ffffff").attr("fill-opacity", 0)
          .on("mousemove", function (event) { showTip(event, b); rectEl.attr("fill-opacity", Math.min(1, BX.alpha + 0.2)); })
          .on("mouseleave", function () { tip.hide(); rectEl.attr("fill-opacity", BX.alpha); });
      });

      // Axlar
      var axisColor = hasFacets ? "#b3b3b3" : "#000000", axisW = hasFacets ? 0.85 : 1.42;
      if (spec.axis_line.x) g.append("line").attr("x1", rect.x0).attr("x2", rect.x1).attr("y1", rect.y1).attr("y2", rect.y1)
        .attr("stroke", axisColor).attr("stroke-width", axisW);
      if (spec.axis_line.y) g.append("line").attr("x1", rect.x0).attr("x2", rect.x0).attr("y1", rect.y0).attr("y2", rect.y1)
        .attr("stroke", axisColor).attr("stroke-width", axisW);
      if (HOR) {
        // x_angle roterar kategoriaxelns text även när lådorna ligger (som axis.text.y i func_graf_ladagram)
        g.selectAll(null).data(leftLabels && spec.has_categories ? cats : []).enter().append("text")
          .attr("transform", function (c) {
            return "translate(" + (rect.x0 - pt(3)) + "," + (band(c) + step / 2) + ")" + (spec.x_angle ? " rotate(" + (-spec.x_angle) + ")" : "");
          }).attr("dy", "0.35em")
          .attr("text-anchor", "end").attr("fill", "#4d4d4d")
          .style("font-size", catSize + "px").style("font-family", font).text(function (c) { return c; });
        g.selectAll(null).data(vt).enter().append("text")
          .attr("x", function (t) { return vs(t.v); }).attr("y", rect.y1 + pt(3)).attr("dy", "0.85em")
          .attr("text-anchor", "middle").attr("fill", "#4d4d4d")
          .style("font-size", valSize + "px").style("font-family", font).text(function (t) { return t.label; });
      } else {
        g.selectAll(null).data(leftLabels ? vt : []).enter().append("text")
          .attr("x", rect.x0 - pt(3)).attr("y", function (t) { return vs(t.v); }).attr("dy", "0.35em")
          .attr("text-anchor", "end").attr("fill", "#4d4d4d")
          .style("font-size", valSize + "px").style("font-family", font).text(function (t) { return t.label; });
        var cl = g.selectAll(null).data(spec.has_categories ? cats : []).enter().append("text")
          .attr("fill", "#4d4d4d").style("font-size", catSize + "px").style("font-family", font).text(function (c) { return c; });
        if (spec.x_angle > 0) {
          cl.attr("text-anchor", "end").attr("dy", "0.71em").attr("transform", function (c) {
            return "translate(" + (band(c) + step / 2) + "," + (rect.y1 + pt(3)) + ") rotate(" + (-spec.x_angle) + ")";
          });
        } else {
          cl.attr("text-anchor", "middle").attr("x", function (c) { return band(c) + step / 2; })
            .attr("y", rect.y1 + pt(3)).attr("dy", "0.85em");
        }
      }
    }

    function showTip(event, b) {
      if (!TT.show) return;
      var dec = TT.decimals, u = TT.unit;
      var title = spec.has_categories ? b.x : (T.y_axis_title || "Alla värden");
      var html = "<div class='vgr-tt-title'>" + esc(title) + "</div>";
      var sub = [];
      if (spec.has_groups) sub.push(groupByKey[b.g].label);
      if (hasFacets) sub.push(b.f);
      if (spec.filter.enabled) sub.push(state.filter);
      if (sub.length) html += "<div class='vgr-tt-sub'>" + esc(sub.join(" · ")) + "</div>";
      html += vgrD3.tooltipRow("Antal", fmt(b.n, 0));
      html += vgrD3.tooltipRow("Övre morrhår", fmt(b.upper, dec) + u);
      html += vgrD3.tooltipRow("Tredje kvartilen", fmt(b.q3, dec) + u);
      html += vgrD3.tooltipRow("Median", fmt(b.median, dec) + u, spec.has_groups ? groupByKey[b.g].color : null);
      html += vgrD3.tooltipRow("Första kvartilen", fmt(b.q1, dec) + u);
      html += vgrD3.tooltipRow("Nedre morrhår", fmt(b.lower, dec) + u);
      html += vgrD3.tooltipRow("Medelvärde", fmt(b.mean, dec) + u);
      if (BX.outliers) html += vgrD3.tooltipRow("Avvikande värden", fmt(b.n_out, 0));
      tip.show(html, event);
    }

    // -------------------------------------------------------------------------
    //  Kontroller: filter med uppspelning, sortering, visa alla, export
    // -------------------------------------------------------------------------
    var timer = null, playBtn = null, filterSel = null;
    function stopPlay() { if (timer) { timer.stop(); timer = null; } if (playBtn) playBtn.text("▶"); }
    function setFilter(k) { var from = state.prevStats; state.filter = k; if (filterSel) filterSel.property("value", k); draw(from); }
    if (spec.filter.enabled) {
      if (spec.filter.label) controls.append("span").style("color", "#546e7a").text(spec.filter.label + ":");
      filterSel = controls.append("select").attr("aria-label", spec.filter.label || "Filter")
        .style("font", "inherit").style("padding", "3px 6px").style("border", "1px solid #b0bec5").style("border-radius", "4px")
        .on("change", function () { stopPlay(); setFilter(this.value); });
      filterSel.selectAll("option").data(spec.filter.values).join("option")
        .attr("value", function (d) { return d; }).text(function (d) { return d; });
      filterSel.property("value", state.filter);
      playBtn = vgrD3.button(controls, "▶", function () {
        if (timer) { stopPlay(); return; }
        var vals = spec.filter.values;
        if (vals.indexOf(state.filter) >= vals.length - 1) setFilter(vals[0]);
        playBtn.text("❚❚");
        timer = d3.interval(function () {
          var i = vals.indexOf(state.filter);
          if (i >= vals.length - 1) { stopPlay(); return; }
          setFilter(vals[i + 1]);
        }, spec.animation.play_interval);
      }, "Spela upp");
    }
    if (IA.sort) {
      var sortSel = controls.append("select").attr("aria-label", "Sortera lådorna")
        .style("font", "inherit").style("padding", "3px 6px").style("border", "1px solid #b0bec5").style("border-radius", "4px")
        .on("change", function () { state.sort = this.value; draw(); });
      sortSel.selectAll("option").data([
        { k: "original", t: "Standardordning" }, { k: "value_desc", t: "Median, högst först" },
        { k: "value_asc", t: "Median, lägst först" }, { k: "name_asc", t: "Namn A–Ö" }, { k: "name_desc", t: "Namn Ö–A" }
      ]).join("option").attr("value", function (d) { return d.k; }).text(function (d) { return d.t; });
      sortSel.property("value", state.sort);
    }
    var dynamic = controls.append("span").style("display", "inline-flex").style("gap", "8px");
    controls.append("span").attr("class", "vgr-spacer");
    if (IA.export) vgrD3.exportButtons(controls, svg.node(), fileName);
    function refreshControls() {
      dynamic.selectAll("*").remove();
      if (Object.keys(state.hidden).length) vgrD3.button(dynamic, "Visa alla", function () { state.hidden = {}; draw(); });
    }
    if (controls.selectAll("button,select").empty()) controls.style("display", "none");

    draw();
  };
})();
