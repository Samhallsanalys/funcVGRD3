/* vgr_D3 — hantel-diagram (func_d3_dumbbell).
 *
 * En rad per kategori (och facett) med start- och slutpunkt. Raderna kopplas med nyckeln
 * facett|kategori så att sortering animeras. Kategoriaxeln följer ggplot2: första kategorin
 * längst ned i liggande läge (scale_y_discrete) och längst till vänster i stående läge.
 */
(function () {
  "use strict";
  var vgrD3 = window.vgrD3;
  function pt(v) { return v * 96 / 72; }
  var LINES = pt(11 * 1.2);

  vgrD3.renderers.dumbbell = function (el, spec) {
    var W = spec.width, H = spec.height, font = spec.font;
    var T = spec.text, F = spec.facets, L = spec.legend, LB = spec.labels, TT = spec.tooltip;
    var IA = spec.interactive, HLS = spec.highlight, PT = spec.point, LN = spec.line;
    var HOR = spec.horizontal;
    var dur = spec.animation.duration || 0;
    var esc = vgrD3.esc;
    var fileName = vgrD3.slug((T.title && T.title.join(" ")) || "dumbbell");

    var root = d3.select(el).style("max-width", W + "px");
    var controls = vgrD3.controls(el, font);
    var svg = root.append("svg").attr("class", "vgr-d3-svg")
      .attr("width", W).attr("height", H).attr("viewBox", "0 0 " + W + " " + H)
      .attr("role", "img").attr("aria-label", (T.title && T.title.join(" ")) || "Hantel-diagram")
      .attr("font-family", font)
      .style("width", "100%").style("height", "auto").style("display", "block");
    var tip = vgrD3.tooltip(el, font);

    var rows = spec.rows;
    rows.forEach(function (r) { r.key = r.f + "|" + r.x; });
    var hasFacets = !(F.levels.length === 1 && F.levels[0] === "__alla");
    var state = { order: spec.order.slice(), sort: spec.sort.by + "|" + spec.sort.direction, first: true, hidden: [false, false] };

    var symbolType = { 15: d3.symbolSquare, 17: d3.symbolTriangle, 18: d3.symbolDiamond }[PT.shape] || d3.symbolCircle;
    var symbolPath = d3.symbol().type(symbolType).size(Math.PI * PT.r * PT.r)();

    // -------------------------------------------------------------------------
    //  Sortering (samma regler som i R: stabil, NA sist, text i C-ordning)
    // -------------------------------------------------------------------------
    function sortOrder(by, dir) {
      if (by === "original") {
        var seen = {}, out = [];
        rows.slice().sort(function (a, b) { return a.o - b.o; }).forEach(function (r) { if (!seen[r.x]) { seen[r.x] = true; out.push(r.x); } });
        return out;
      }
      var desc = dir === "desc";
      var val = function (r) {
        return by === "alphabetical" ? r.x : by === "range" ? (r.change == null ? null : Math.abs(r.change)) : by === "start_value" ? r.start
             : by === "change" ? r.change : r.end;
      };
      var sorted = rows.slice().sort(function (a, b) {
        var va = val(a), vb = val(b);
        var na = va == null || (typeof va === "number" && isNaN(va)), nb = vb == null || (typeof vb === "number" && isNaN(vb));
        if (na || nb) return (na - nb) || (a.o - b.o);
        var c = va < vb ? -1 : va > vb ? 1 : 0;
        return (desc ? -c : c) || (a.o - b.o);
      });
      var seen2 = {}, res = [];
      sorted.forEach(function (r) { if (!seen2[r.x]) { seen2[r.x] = true; res.push(r.x); } });
      return res;
    }

    // -------------------------------------------------------------------------
    //  Etiketter: urval och "intelligent" förskjutning som i func_graf_dumbbell
    // -------------------------------------------------------------------------
    var labelVals = LB.items.map(function (it) { var r = rows[it.rad]; return it.typ === "start" ? r.start : r.end; });
    // Bara värdeaxelns nudge styr avståndet; den andra är i kategorienheter (liggande nudge_y, stående nudge_x)
    var labelRange = ((d3.max(labelVals) - d3.min(labelVals)) || 0) * 0.03;
    var dynNudge = HOR ? Math.max(labelRange, Math.abs(LB.nudge_x))
                       : Math.max(labelRange, Math.abs(LB.nudge_y));
    // Pilens längd: 2 % av största absolutvärdet (samma i alla paneler, som i originalet)
    var arrowOff = (d3.max(rows, function (r) { return Math.max(Math.abs(r.start || 0), Math.abs(r.end || 0)); }) || 0) * 0.02;
    // it.s = sida från R (data_label_side): -1 vänster/under, 1 höger/över
    function labelOffset(it) {
      var r = rows[it.rad];
      var sgn = it.s != null ? it.s : (it.typ === "start" ? -1 : 1);
      // Alltid minst 3 % åt etikettens sida (standardvärdet nudge_y = 0 lade annars stående etiketter på punkten)
      var val = sgn * dynNudge;
      // Slutpunktens etikett på samma sida som riktningspilen flyttas förbi pilen (pil + liten lucka)
      if (spec.arrows.show && it.typ === "end" && r.change != null && Math.abs(r.change) > spec.arrows.threshold &&
          Math.sign(r.change) === sgn) val += sgn * 1.5 * arrowOff;
      return { val: val, cat: HOR ? LB.nudge_y : LB.nudge_x, align: sgn };
    }
    var fmtLabel = function (v) { return vgrD3.fmt(v, 1, " "); };
    // Etiketternas verkliga storlek (som labelNodes mäter dem) — används för axelutrymmet
    var labPad = LB.box && !LB.glow ? LB.size * 0.25 : 1;
    var labGlow = LB.glow ? LB.glow_width : 0;
    LB.items.forEach(function (it) {
      var r = rows[it.rad], v = it.typ === "start" ? r.start : r.end;
      it.w = vgrD3.measureText(svg, fmtLabel(v), LB.size, font) + 2 * labPad + labGlow;
      it.h = LB.size * 1.2 + 2 * labPad + labGlow;
    });
    var fmtTip = function (v) { return vgrD3.fmt(v, TT.decimals, " ") + TT.unit; };

    // -------------------------------------------------------------------------
    //  Fasta lager och rubriker
    // -------------------------------------------------------------------------
    svg.append("rect").attr("width", W).attr("height", H).attr("fill", "#ffffff");
    var m = pt(5.5);
    var box = { x0: m, y0: m, x1: W - m, y1: H - m };
    var headerH = 0;
    var th = vgrD3.textBlock(svg, T.title, { x: box.x0, y: box.y0, size: T.title_size, font: font, anchor: "start", lineHeight: 1.1 });
    headerH += th + (th ? pt(5) : 0);
    var sh = vgrD3.textBlock(svg, T.subtitle, { x: box.x0, y: box.y0 + headerH, size: T.subtitle_size, font: font, anchor: "start" });
    headerH += sh + (sh ? pt(5) : 0);
    if (th || sh) headerH += pt(3);
    var capH = T.caption.length * T.caption_size * 1.2;
    var dynG = svg.append("g");
    var panelsG = svg.append("g");
    var hoverG = svg.append("g").attr("pointer-events", "none");

    // Legend med punktsymboler
    function drawLegend(area) {
      if (L.position === "none") return;
      var key = pt(17.28), rowH = Math.max(L.text_size * 1.3, key);
      var vertical = L.position === "left" || L.position === "right";
      var entries = spec.periods.map(function (p, i) {
        return { label: p, color: spec.colors[i], w: key + 6 + vgrD3.measureText(svg, p, L.text_size, font) };
      });
      var titleW = L.title ? vgrD3.measureText(svg, L.title, L.title_size, font) : 0;
      var titleH = L.title && vertical ? L.title_size * 1.6 : 0;
      var lw, lh;
      if (vertical) {
        entries.forEach(function (e, i) { e.x = 0; e.y = titleH + i * rowH; });
        lw = Math.max(titleW, d3.max(entries, function (e) { return e.w; })) + 8; lh = titleH + entries.length * rowH;
      } else {
        var x = L.title ? titleW + 10 : 0;
        entries.forEach(function (e) { e.x = x; e.y = 0; x += e.w + 16; });
        lw = x - 16; lh = rowH;
      }
      var lx, ly, gap = pt(11);
      if (L.position === "right") { lx = area.x1 - lw; ly = (area.y0 + area.y1) / 2 - lh / 2; area.x1 -= lw + gap; }
      else if (L.position === "left") { lx = area.x0; ly = (area.y0 + area.y1) / 2 - lh / 2; area.x0 += lw + gap; }
      else if (L.position === "top") { lx = (area.x0 + area.x1) / 2 - lw / 2; ly = area.y0; area.y0 += lh + gap; }
      else { lx = (area.x0 + area.x1) / 2 - lw / 2; ly = area.y1 - lh; area.y1 -= lh + gap; }
      var lg = dynG.append("g").attr("transform", "translate(" + lx + "," + ly + ")");
      if (L.title) {
        lg.append("text").attr("y", vertical ? L.title_size : rowH / 2).attr("dy", vertical ? null : "0.35em")
          .style("font-size", L.title_size + "px").style("font-family", font).text(L.title);
      }
      var item = lg.selectAll(null).data(entries).enter().append("g").attr("class", "vgr-leg-item")
        .attr("transform", function (e) { return "translate(" + e.x + "," + e.y + ")"; })
        .attr("opacity", function (e, i) { return state.hidden[i] ? 0.35 : 1; });
      if (IA.legend) {
        // Klick döljer/visar periodens punkter; hovring tonar ned den andra perioden
        // Den sista synliga perioden går inte att dölja (som i stapeldiagrammet)
        item.append("title").text("Klicka för att dölja/visa");
        item.style("cursor", "pointer")
          .on("click", function (event, e) {
            var i = entries.indexOf(e);
            if (!state.hidden[i] && state.hidden[1 - i]) return;
            state.hidden[i] = !state.hidden[i];
            // Hovringens nedtoning nollställs, annars blir den kvar när legenden ritas om
            panelsG.selectAll(".vgr-start, .vgr-end, .vgr-link, .vgr-arrow").attr("opacity", null);
            update(false);
          })
          .on("mouseenter", function (event, e) {
            if (state.hidden[0] || state.hidden[1]) return;
            panelsG.selectAll(entries.indexOf(e) === 0 ? ".vgr-end" : ".vgr-start").attr("opacity", 0.15);
            panelsG.selectAll(".vgr-link, .vgr-arrow").attr("opacity", 0.3);
          })
          .on("mouseleave", function () { panelsG.selectAll(".vgr-start, .vgr-end, .vgr-link, .vgr-arrow").attr("opacity", null); });
        item.append("rect").attr("width", function (e) { return e.w; }).attr("height", rowH).attr("fill", "#ffffff").attr("fill-opacity", 0);
      }
      item.append("path").attr("d", symbolPath).attr("transform", "translate(" + key / 2 + "," + rowH / 2 + ")")
        .attr("fill", function (e) { return e.color; }).attr("fill-opacity", PT.alpha);
      item.append("text").attr("x", key + 6).attr("y", rowH / 2).attr("dy", "0.35em")
        .style("font-size", L.text_size + "px").style("font-family", font).text(function (e) { return e.label; });
    }

    // -------------------------------------------------------------------------
    //  Uppdatering
    // -------------------------------------------------------------------------
    function update(animate) {
      var t = animate && dur > 0 ? dur : 0;
      dynG.selectAll("*").remove();
      hoverG.selectAll("*").remove();
      tip.hide();

      var area = { x0: box.x0, y0: box.y0 + headerH, x1: box.x1, y1: box.y1 - (capH ? capH + pt(5.5) : 0) };
      drawLegend(area);
      if (T.x_axis_title) {
        dynG.append("text").attr("text-anchor", "middle")
          .attr("transform", "translate(" + ((area.x0 + area.x1) / 2) + "," + (area.y1 - T.axis_title_size * 0.3) + ") rotate(" + (-T.x_axis_title_angle) + ")")
          .style("font-size", T.axis_title_size + "px").style("font-family", font).text(T.x_axis_title);
        area.y1 -= T.axis_title_size * 1.6;
      }
      if (T.y_axis_title) {
        dynG.append("text").attr("text-anchor", "middle")
          .attr("transform", "translate(" + (area.x0 + T.axis_title_size * 0.9) + "," + ((area.y0 + area.y1) / 2) + ") rotate(-90)")
          .style("font-size", T.axis_title_size + "px").style("font-family", font).text(T.y_axis_title);
        area.x0 += T.axis_title_size * 1.6;
      }

      // Kategorier per panel: fria kategorier om liggande och facet_same_y_scale = FALSE
      var freeCats = hasFacets && !F.same && HOR;
      var freeVals = hasFacets && !F.same && !HOR;
      var panelInfo = F.levels.map(function (f) {
        var pr = rows.filter(function (r) { return r.f === f; });
        var present = {};
        pr.forEach(function (r) { present[r.x] = true; });
        var cats = freeCats ? state.order.filter(function (c) { return present[c]; }) : state.order;
        return { f: f, rows: pr, cats: cats };
      });

      function valueDomain(rs) {
        var vals = [];
        rs.forEach(function (r) {
          if (r.start != null) vals.push(r.start);
          if (r.end != null) vals.push(r.end);
        });
        var lo = d3.min(vals), hi = d3.max(vals);
        if (lo == null) { lo = 0; hi = 1; }
        var all = rows.map(function (r) { return Math.max(Math.abs(r.start || 0), Math.abs(r.end || 0)); });
        var arrowOff = (d3.max(all) || 0) * 0.02;
        if (spec.arrows.show) {
          rs.forEach(function (r) {
            if (r.change != null && Math.abs(r.change) > spec.arrows.threshold && r.end != null) {
              var e = r.end + Math.sign(r.change) * arrowOff;
              lo = Math.min(lo, e); hi = Math.max(hi, e);
            }
          });
        }
        if (spec.baseline.show) { lo = Math.min(lo, 0); hi = Math.max(hi, 0); }
        if (LB.show && !LB.repel) {
          LB.items.forEach(function (it) {
            var r = rows[it.rad];
            if (rs.indexOf(r) < 0) return;
            var v = (it.typ === "start" ? r.start : r.end) + labelOffset(it).val;
            lo = Math.min(lo, v); hi = Math.max(hi, v);
          });
        }
        if (lo === hi) { lo -= 1; hi += 1; }
        var span = hi - lo;
        // Innehållet som axelns brytpunkter ska täcka: datan, plus 0 när nollinjen ritas
        var content = [d3.min(vals), d3.max(vals)];
        if (content[0] == null) content = [lo, hi];
        if (spec.baseline.show) content = [Math.min(content[0], 0), Math.max(content[1], 0)];
        return { dom: [lo - span * 0.05, hi + span * 0.05], arrowOff: arrowOff, dataRange: [d3.min(vals), d3.max(vals)],
                 content: content, padded: false };
      }

      // Utrymme för etiketter utan repel (data_label_axis_padding): etiketten ligger på sin sida om punkten
      // och sträcker sig sin egen bredd (liggande) / höjd (stående) utåt. Värdeaxeln breddas tills alla
      // etiketter får plats i panelen (lenPx = panelens längd längs värdeaxeln). Eftersom skalan krymper
      // när domänen växer upprepas det tills det står still. Datan behåller minst 25 % av panelen.
      function padDomain(vinfo, rs, lenPx) {
        if (!LB.show || LB.repel || !(LB.axis_padding > 0) || !(lenPx > 0)) return vinfo;
        var its = LB.items.filter(function (it) { return rs.indexOf(rows[it.rad]) >= 0; });
        if (!its.length) return vinfo;
        var lo0 = vinfo.dom[0], hi0 = vinfo.dom[1], lo = lo0, hi = hi0;
        var maxSpan = 4 * ((vinfo.content[1] - vinfo.content[0]) || (hi0 - lo0));
        for (var k = 0; k < 50; k++) {
          var perPx = (hi - lo) / lenPx, nlo = lo0, nhi = hi0;
          its.forEach(function (it) {
            var off = labelOffset(it), r = rows[it.rad];
            var v = (it.typ === "start" ? r.start : r.end) + off.val;
            var ext = ((HOR ? it.w : it.h) * LB.axis_padding + pt(2)) * perPx;
            if (off.align < 0) nlo = Math.min(nlo, v - ext); else nhi = Math.max(nhi, v + ext);
          });
          if (nhi - nlo > maxSpan && maxSpan > hi0 - lo0) {
            // Taket: dela det tillåtna extra utrymmet i proportion till vad sidorna behöver
            var f = (maxSpan - (hi0 - lo0)) / ((lo0 - nlo) + (nhi - hi0));
            nlo = lo0 - (lo0 - nlo) * f; nhi = hi0 + (nhi - hi0) * f;
            lo = nlo; hi = nhi; break;
          }
          var done = Math.abs(nlo - lo) + Math.abs(nhi - hi) < 1e-9 * (hi - lo);
          lo = nlo; hi = nhi;
          if (done) break;
        }
        return { dom: [lo, hi], arrowOff: vinfo.arrowOff, dataRange: vinfo.dataRange, content: vinfo.content,
                 padded: lo < lo0 || hi > hi0 };
      }

      // Brytpunkter på värdeaxeln. Breddad axel: bara inom innehållet (inte ut i etikettutrymmet), och
      // glesas ut (varannan, var fjärde ...) tills det är högst 7 och axeltexterna inte krockar
      function valueTicks(vinfo, lenPx, size) {
        if (!vinfo.padded) return d3.scaleLinear().domain(vinfo.dom).ticks(5);
        var lo = Math.max(vinfo.dom[0], vinfo.content[0]), hi = Math.min(vinfo.dom[1], vinfo.content[1]);
        if (!(hi > lo)) { lo = vinfo.dom[0]; hi = vinfo.dom[1]; }
        var tks = d3.ticks(lo, hi, 5);
        var perPx = lenPx / (vinfo.dom[1] - vinfo.dom[0]);
        for (var k = 0; k < 10 && tks.length > 2; k++) {
          var step = tks[1] - tks[0], dec = d3.precisionFixed(step);
          var need = HOR ? d3.max(tks, function (tk) { return vgrD3.measureText(svg, vgrD3.fmt(tk, dec, " "), size, font); }) + pt(4)
                         : size * 1.3;
          if (tks.length <= 7 && step * perPx >= need) break;
          // Behåll multiplerna av det dubbla steget (2, 4, 6 hellre än 3, 5, 7), annars varannan
          var on = tks.filter(function (tk) { var q = tk / (2 * step); return Math.abs(q - Math.round(q)) < 1e-8; });
          tks = on.length >= 2 ? on : tks.filter(function (tk, j) { return j % 2 === 0; });
        }
        return tks;
      }

      // Panelrutnät (facet_grid med en kolumn: höjd efter antal kategorier)
      var n = F.levels.length, ncol = Math.min(F.ncol, n), nrow = Math.ceil(n / ncol);
      var sp = hasFacets ? 0.5 * LINES : 0;
      var stripH = hasFacets ? T.facet_title_size * 1.2 + pt(8) : 0;
      var cellW = (area.x1 - area.x0 - (ncol - 1) * sp) / ncol;

      // Axelmått
      var catSize = HOR ? T.y_axis_text_size : T.x_axis_text_size;
      var valSize = HOR ? T.x_axis_text_size : T.y_axis_text_size;
      var catW = d3.max(state.order, function (c) { return vgrD3.measureText(svg, c, catSize, font); }) || 0;
      var bottomH = (HOR ? valSize : catSize) + pt(5);
      // Nederaxeln (x är aldrig fri) bara på paneler utan panel under sig, som ggplot facet_wrap/facet_grid.
      // Rader utan någon sådan panel får inget utrymme för axeltext
      function bottomLabels(i) { return !hasFacets || i + ncol >= n; }
      var rowBottom = d3.range(nrow).map(function (r) {
        return d3.range(r * ncol, Math.min(n, (r + 1) * ncol)).some(bottomLabels) ? bottomH : 0;
      });
      var totalH = area.y1 - area.y0 - (nrow - 1) * sp - d3.sum(rowBottom);
      var weights = panelInfo.map(function (pi) { return F.grid_rows ? pi.cats.length + 1.2 : 1; });
      var rowHeights = [];
      for (var rr = 0; rr < nrow; rr++) {
        var wsum = 0, wall = d3.sum(weights);
        for (var cc = 0; cc < ncol; cc++) { var ii = rr * ncol + cc; if (ii < n) wsum = Math.max(wsum, weights[ii]); }
        rowHeights.push((F.grid_rows ? totalH * wsum / wall : totalH / nrow) + rowBottom[rr]);
      }
      // Delad vänsteraxel (samma kategorier i alla paneler när liggande, samma värdeskala när stående):
      // axeltexterna bara i första kolumnen och panelerna delar på den frigjorda bredden (som ggplot)
      var leftShared = hasFacets && ncol > 1 && (HOR ? !freeCats : !freeVals);
      // Panelens ritbredd (utan vänstermarginal) när vänsteraxeln är delad; leftW sätts nedan
      function sharedPanelW(lw) { return (area.x1 - area.x0 - lw - (ncol - 1) * sp) / ncol; }
      // Panelens längd längs värdeaxeln: bredd (liggande; vänstermarginalen = kategoritexterna) eller höjd
      function valueLen(prow) {
        if (!HOR) return rowHeights[prow] - stripH - rowBottom[prow];
        return leftShared ? sharedPanelW(catW + pt(4)) : cellW - catW - pt(4);
      }
      // Gemensam skala: alla paneler får samma domän, så den räknas för den kortaste panelen
      var sharedVal = freeVals ? null
        : padDomain(valueDomain(rows), rows, d3.min(d3.range(nrow), function (pr) { return valueLen(pr); }));
      var panelV = panelInfo.map(function (pi, i) {
        return sharedVal || padDomain(valueDomain(pi.rows), pi.rows, valueLen(Math.floor(i / ncol)));
      });
      var valW = 0;
      panelInfo.forEach(function (pi, i) {
        var tks = valueTicks(panelV[i], valueLen(Math.floor(i / ncol)), valSize);
        var dec = tks.length > 1 ? d3.precisionFixed(tks[1] - tks[0]) : 0;
        tks.forEach(function (tk) {
          valW = Math.max(valW, vgrD3.measureText(svg, vgrD3.fmt(tk, dec, " "), valSize, font));
        });
      });
      var leftW = (HOR ? catW : valW) + pt(4);

      var panels = panelsG.selectAll("g.vgr-panel").data(panelInfo, function (pi) { return pi.f; })
        .join(function (enter) {
          var g = enter.append("g").attr("class", "vgr-panel");
          ["vgr-bg", "vgr-grid", "vgr-rows", "vgr-axes", "vgr-labels"].forEach(function (c) { g.append("g").attr("class", c); });
          g.append("clipPath").append("rect");   // etiketter klipps mot panelen om inte x_axis_clip_off
          return g;
        });
      var panelRight = area.x0;

      panels.each(function (pi, i) {
        var g = d3.select(this);
        var col = i % ncol, prow = Math.floor(i / ncol);
        var cx = area.x0 + col * (cellW + sp);
        var cy = area.y0 + d3.sum(rowHeights.slice(0, prow)) + prow * sp;
        var cellH = rowHeights[prow];
        var rect = { x0: cx + leftW, y0: cy + stripH, x1: cx + cellW, y1: cy + cellH - rowBottom[prow] };
        if (leftShared) {
          var pw = sharedPanelW(leftW);
          rect.x0 = area.x0 + leftW + col * (pw + sp);
          rect.x1 = rect.x0 + pw;
        }
        var leftLabels = !leftShared || col === 0;
        panelRight = Math.max(panelRight, rect.x1);
        var vinfo = panelV[i];

        // Diskret kategoriaxel: position 1..n med 0,6 enheters marginal (ggplot2-standard)
        var idx = {};
        pi.cats.forEach(function (c, k) { idx[c] = k + 1; });
        var nc = Math.max(pi.cats.length, 1);
        // Stående utan repel: etiketterna ligger mitt på sin punkt, så den bredaste behöver halva sin bredd
        // utanför första/sista kategorin (som func_graf_dumbbell sedan 2026-10-09; här uppmätt). Marginalen
        // växer bara från 0,6 och aldrig förbi att kategorierna får hälften av panelen
        var catMargin = 0.6;
        if (!HOR && LB.show && !LB.repel && LB.axis_padding > 0 && !LB.clip_off) {
          var labelW = 0;   // bredaste etiketten i panelen (inte catW ovan, som är kategoritextens bredd)
          LB.items.forEach(function (it) {
            if (Object.prototype.hasOwnProperty.call(idx, rows[it.rad].x)) labelW = Math.max(labelW, it.w);   // (inte ärvda namn som "constructor")
          });
          var halfW = (labelW / 2) * LB.axis_padding + pt(2), lenX = rect.x1 - rect.x0;
          for (var q = 0; q < 10 && labelW > 0; q++) {   // (stående utan repel flyttas etiketten inte i sidled)
            var unitPx = lenX / (nc - 1 + 2 * catMargin);
            catMargin = Math.min(Math.max(0.6, halfW / unitPx), Math.max(0.6, (nc - 1) / 2));
          }
        }
        var cs = d3.scaleLinear().domain([1 - catMargin, nc + catMargin]).range(HOR ? [rect.y1, rect.y0] : [rect.x0, rect.x1]);
        var unit = Math.abs(cs(1) - cs(0));
        var vs = d3.scaleLinear().domain(vinfo.dom).range(HOR ? [rect.x0, rect.x1] : [rect.y1, rect.y0]);
        var ticks = valueTicks(vinfo, HOR ? rect.x1 - rect.x0 : rect.y1 - rect.y0, valSize);
        var tdec = ticks.length > 1 ? d3.precisionFixed(ticks[1] - ticks[0]) : 0;
        // Punkt i skärmkoordinater: (kategori, värde)
        function P(cat, v) { return HOR ? [vs(v), cs(idx[cat])] : [cs(idx[cat]), vs(v)]; }

        // Bakgrund: rutnät, ram, facettitel
        var grid = g.select(".vgr-grid");
        grid.selectAll("*").remove();
        if (hasFacets) {
          grid.append("text").attr("x", (rect.x0 + rect.x1) / 2).attr("y", cy + T.facet_title_size).attr("text-anchor", "middle")
            .style("font-size", T.facet_title_size + "px").style("font-family", font).text(pi.f);
          grid.append("rect").attr("x", rect.x0).attr("y", rect.y0).attr("width", rect.x1 - rect.x0).attr("height", rect.y1 - rect.y0)
            .attr("fill", "none").attr("stroke", "#cccccc").attr("stroke-width", 1.42);
        }
        function valLine(v, color, w) {
          var p = vs(v);
          grid.append("line").attr("x1", HOR ? p : rect.x0).attr("x2", HOR ? p : rect.x1)
            .attr("y1", HOR ? rect.y0 : p).attr("y2", HOR ? rect.y1 : p).attr("stroke", color).attr("stroke-width", w);
        }
        // true/false eller "x"/"y" (linjer från den visade x- resp. y-axeln = lodräta resp. vågräta).
        // Värdelinjerna är lodräta på liggande dumbbells, kategorilinjerna vågräta
        function gridOn(setting, vertical) { return setting === true || (setting === "x" && vertical) || (setting === "y" && !vertical); }
        var GMIN = spec.grid.minor, GMAJ = spec.grid.major;
        if (gridOn(GMIN, HOR)) for (var k = 1; k < ticks.length; k++) valLine((ticks[k - 1] + ticks[k]) / 2, "#f2f2f2", 0.85);
        if (gridOn(GMAJ, HOR)) ticks.forEach(function (tk) { valLine(tk, "#e5e5e5", 1.42); });
        if (gridOn(GMAJ, !HOR)) {
          pi.cats.forEach(function (c) {
            var p = cs(idx[c]);
            grid.append("line").attr("x1", HOR ? rect.x0 : p).attr("x2", HOR ? rect.x1 : p)
              .attr("y1", HOR ? p : rect.y0).attr("y2", HOR ? p : rect.y1).attr("stroke", "#e5e5e5").attr("stroke-width", 1.42);
          });
        }
        if (spec.baseline.show) {
          var bp = vs(0);
          grid.append("line").attr("x1", HOR ? bp : rect.x0).attr("x2", HOR ? bp : rect.x1)
            .attr("y1", HOR ? rect.y0 : bp).attr("y2", HOR ? rect.y1 : bp)
            .attr("stroke", spec.baseline.color).attr("stroke-width", spec.baseline.size).attr("opacity", 0.7);
        }

        // Rader: markering, linje, punkter, pilar
        var rowSel = g.select(".vgr-rows").selectAll("g.vgr-row").data(pi.rows.filter(function (r) { return idx[r.x]; }), function (r) { return r.key; });
        rowSel.exit().remove();
        var ent = rowSel.enter().append("g").attr("class", "vgr-row");
        ent.append("rect").attr("class", "vgr-hl");
        ent.append("rect").attr("class", "vgr-sidebar");
        ent.append("line").attr("class", "vgr-link");
        ent.append("path").attr("class", "vgr-arrow");
        ent.append("path").attr("class", "vgr-start");
        ent.append("path").attr("class", "vgr-end");
        ent.append("rect").attr("class", "vgr-hit");
        var all = ent.merge(rowSel);
        var trans = function (sel) { return (state.first ? (spec.animation.draw && dur > 0) : t > 0) ? sel.transition().duration(state.first ? dur : t) : sel; };

        // Markerad rad (bakgrund och sidostapel)
        all.select(".vgr-hl").each(function (r) {
          var s = d3.select(this);
          if (!HLS.background || !r.hl) { s.attr("display", "none"); return; }
          var c = cs(idx[r.x]);
          var lo = HLS.full_width ? null : vs(vinfo.dataRange[0] - (vinfo.dataRange[1] - vinfo.dataRange[0]) * 0.05);
          var hi = HLS.full_width ? null : vs(vinfo.dataRange[1] + (vinfo.dataRange[1] - vinfo.dataRange[0]) * 0.05);
          var a0 = HLS.full_width ? (HOR ? rect.x0 : rect.y0) : Math.min(lo, hi);
          var a1 = HLS.full_width ? (HOR ? rect.x1 : rect.y1) : Math.max(lo, hi);
          s.attr("display", null).attr("fill", HLS.color).attr("fill-opacity", HLS.alpha);
          trans(s).attr("x", HOR ? a0 : c - 0.4 * unit).attr("width", HOR ? a1 - a0 : 0.8 * unit)
            .attr("y", HOR ? c - 0.4 * unit : a0).attr("height", HOR ? 0.8 * unit : a1 - a0);
        });
        all.select(".vgr-sidebar").each(function (r) {
          var s = d3.select(this);
          if (!HLS.bar || !r.hl) { s.attr("display", "none"); return; }
          var c = cs(idx[r.x]);
          var edge = vs(vinfo.dataRange[0] - (vinfo.dataRange[1] - vinfo.dataRange[0]) * HLS.bar_width);
          s.attr("display", null).attr("fill", HLS.bar_color).attr("fill-opacity", HLS.bar_alpha);
          if (HOR) trans(s).attr("x", rect.x0).attr("width", Math.max(0, edge - rect.x0)).attr("y", c - 0.4 * unit).attr("height", 0.8 * unit);
          else trans(s).attr("x", c - 0.4 * unit).attr("width", 0.8 * unit).attr("y", edge).attr("height", Math.max(0, rect.y1 - edge));
        });

        // Linje: vid öppning börjar slutet på startvärdet
        var link = all.select(".vgr-link")
          .attr("stroke", function (r) { return LN.by_change ? (r.change >= 0 ? LN.increase : LN.decrease) : LN.color; })
          .attr("stroke-width", LN.size).attr("stroke-opacity", 0.8)
          .attr("stroke-dasharray", LN.dash ? LN.dash.map(function (v) { return v * LN.size; }).join(" ") : null)
          .attr("display", function (r) { return r.start == null || r.end == null || state.hidden[0] || state.hidden[1] ? "none" : null; });
        var startP = all.select(".vgr-start").attr("d", symbolPath).attr("fill", spec.colors[0]).attr("fill-opacity", PT.alpha)
          .attr("display", function (r) { return r.start == null || state.hidden[0] ? "none" : null; });
        var endP = all.select(".vgr-end").attr("d", symbolPath).attr("fill", spec.colors[1]).attr("fill-opacity", PT.alpha)
          .attr("display", function (r) { return r.end == null || state.hidden[1] ? "none" : null; });
        if (state.first && spec.animation.draw && dur > 0) {
          link.attr("x1", function (r) { return P(r.x, r.start)[0]; }).attr("y1", function (r) { return P(r.x, r.start)[1]; })
            .attr("x2", function (r) { return P(r.x, r.start)[0]; }).attr("y2", function (r) { return P(r.x, r.start)[1]; });
          endP.attr("transform", function (r) { return "translate(" + P(r.x, r.start != null ? r.start : r.end) + ")"; });
        }
        startP.filter(function (r) { return r.start != null; }).each(function (r) {
          var s = d3.select(this);
          (state.first ? s : trans(s)).attr("transform", "translate(" + P(r.x, r.start) + ")");
        });
        trans(endP.filter(function (r) { return r.end != null; })).attr("transform", function (r) { return "translate(" + P(r.x, r.end) + ")"; });
        trans(link.filter(function (r) { return r.start != null && r.end != null; }))
          .attr("x1", function (r) { return P(r.x, r.start)[0]; }).attr("y1", function (r) { return P(r.x, r.start)[1]; })
          .attr("x2", function (r) { return P(r.x, r.end)[0]; }).attr("y2", function (r) { return P(r.x, r.end)[1]; });

        // Riktningspilar
        all.select(".vgr-arrow").each(function (r) {
          var s = d3.select(this);
          if (!spec.arrows.show || state.hidden[0] || state.hidden[1] || r.change == null || !(Math.abs(r.change) > spec.arrows.threshold)) { s.attr("display", "none"); return; }
          var a = P(r.x, r.end), b = P(r.x, r.end + Math.sign(r.change) * vinfo.arrowOff);
          var dx = b[0] - a[0], dy = b[1] - a[1], len = Math.sqrt(dx * dx + dy * dy) || 1, ux = dx / len, uy = dy / len;
          // Pilspetsen (0,15 cm som i originalet) slutar vid b
          var head = 5.7, bx = b[0] - ux * head, by = b[1] - uy * head;
          var d = "M" + a[0] + "," + a[1] + "L" + bx + "," + by +
                  "M" + b[0] + "," + b[1] + "L" + (bx - uy * head * 0.5) + "," + (by + ux * head * 0.5) +
                  "L" + (bx + uy * head * 0.5) + "," + (by - ux * head * 0.5) + "Z";
          s.attr("display", null).attr("d", d)
            .attr("stroke", r.change > 0 ? "#006400" : "#8b0000").attr("fill", r.change > 0 ? "#006400" : "#8b0000")
            // Pilarna tonas bara in när punkterna animeras fram (animation_draw)
            .attr("stroke-width", 2.3).attr("opacity", state.first && spec.animation.draw && dur ? 0 : 1)
            .transition().delay(state.first && spec.animation.draw ? dur : 0).duration(state.first && spec.animation.draw ? 200 : 0).attr("opacity", 1);
        });

        // Träffyta för tooltip (hela raden)
        all.select(".vgr-hit")
          .attr("x", function (r) { return HOR ? rect.x0 : cs(idx[r.x]) - 0.5 * unit; })
          .attr("y", function (r) { return HOR ? cs(idx[r.x]) - 0.5 * unit : rect.y0; })
          .attr("width", HOR ? rect.x1 - rect.x0 : unit).attr("height", HOR ? unit : rect.y1 - rect.y0)
          .attr("fill", "#ffffff").attr("fill-opacity", 0)
          .on("mouseenter mousemove", function (event, r) { hoverRow(r, event, pi.f, HOR, cs(idx[r.x]), unit, rect); })
          .on("mouseleave", function () { hoverG.selectAll("*").remove(); tip.hide(); });

        // Axlar
        var axes = g.select(".vgr-axes");
        axes.selectAll("*").remove();
        var AXL = spec.axis_line || { x: true, y: true };   // x_axis_line / y_axis_line (standard: båda, som tidigare)
        if (AXL.x) axes.append("line").attr("x1", rect.x0).attr("x2", rect.x1).attr("y1", rect.y1).attr("y2", rect.y1).attr("stroke", "#4d4d4d").attr("stroke-width", 1.42);
        if (AXL.y) axes.append("line").attr("x1", rect.x0).attr("x2", rect.x0).attr("y1", rect.y0).attr("y2", rect.y1).attr("stroke", "#4d4d4d").attr("stroke-width", 1.42);
        var valLabels = ticks.map(function (tk) { return { p: vs(tk), label: vgrD3.fmt(tk, tdec, " ") }; });
        var catLabels = pi.cats.map(function (c) { return { p: cs(idx[c]), label: c, key: c }; });
        var leftL = leftLabels ? (HOR ? catLabels : valLabels) : [];
        var bottomL = bottomLabels(i) ? (HOR ? valLabels : catLabels) : [];
        var lsel = axes.selectAll("text.vgr-left").data(leftL).enter().append("text").attr("class", "vgr-left")
          .attr("x", rect.x0 - pt(3)).attr("dy", "0.35em").attr("text-anchor", "end").attr("fill", "#4d4d4d")
          .style("font-size", (HOR ? catSize : valSize) + "px").style("font-family", font).text(function (d) { return d.label; });
        var bsel = axes.selectAll("text.vgr-bottom").data(bottomL).enter().append("text").attr("class", "vgr-bottom")
          .attr("y", rect.y1 + pt(3)).attr("dy", "0.85em").attr("text-anchor", "middle").attr("fill", "#4d4d4d")
          .style("font-size", (HOR ? valSize : catSize) + "px").style("font-family", font).text(function (d) { return d.label; });
        // Kategorietiketter glider till sin nya plats vid sortering
        var prevPos = g.property("__catpos") || {};
        var newPos = {};
        catLabels.forEach(function (d) { newPos[d.key] = d.p; });
        g.property("__catpos", newPos);
        if (HOR) {
          bsel.attr("x", function (d) { return d.p; });
          lsel.attr("y", function (d) { return d.key != null && prevPos[d.key] != null && t ? prevPos[d.key] : d.p; });
          if (t) lsel.transition().duration(t).attr("y", function (d) { return d.p; });
        } else {
          lsel.attr("y", function (d) { return d.p; });
          bsel.attr("x", function (d) { return d.key != null && prevPos[d.key] != null && t ? prevPos[d.key] : d.p; });
          if (t) bsel.transition().duration(t).attr("x", function (d) { return d.p; });
        }

        // Dataetiketter
        var labG = g.select(".vgr-labels");
        labG.selectAll("*").remove();
        // x_axis_clip_off = FALSE: etiketter klipps vid panelkanten som i ggplot (TRUE: ritas hela, även utanför)
        var clipId = el.id + "-dbclip-" + i;
        g.select("clipPath").attr("id", clipId).select("rect")
          .attr("x", rect.x0).attr("y", rect.y0).attr("width", Math.max(0, rect.x1 - rect.x0)).attr("height", Math.max(0, rect.y1 - rect.y0));
        labG.attr("clip-path", LB.clip_off ? null : "url(#" + clipId + ")");
        if (LB.show) {
          var items = [];
          LB.items.forEach(function (it) {
            var r = rows[it.rad];
            if (r.f !== pi.f || !idx[r.x] || state.hidden[it.typ === "start" ? 0 : 1]) return;
            var v = it.typ === "start" ? r.start : r.end;
            var off = labelOffset(it);
            // Förskjutning i kategoririktningen: alltid för liggande, bara med repel för stående (som originalet)
            var catPos = idx[r.x] + (HOR || LB.repel ? off.cat : 0);
            var p = HOR ? [vs(v + off.val), cs(catPos)] : [cs(catPos), vs(v + off.val)];
            var o = P(r.x, v);
            items.push({ text: fmtLabel(v), x: p[0], y: p[1], ax: p[0], ay: p[1], ox: o[0], oy: o[1], align: off.val !== 0 || !LB.repel ? off.align : 0 });
          });
          var nodes = vgrD3.labelNodes(labG, items, {
            size: LB.size, font: font, color: LB.color, box: LB.box && !LB.glow, box_color: LB.box_color, box_alpha: LB.box_alpha,
            glow: LB.glow, glow_color: LB.glow_color, glow_width: LB.glow_width, angle: LB.angle
          });
          if (!LB.repel) {
            // hjust/vjust som originalet: start till vänster/under, slut till höger/över
            items.forEach(function (d) {
              if (HOR) d.x += d.align * d.w / 2; else d.y -= d.align * d.h / 2;
            });
          } else {
            var points = [];
            pi.rows.forEach(function (r) {
              if (!idx[r.x]) return;
              if (r.start != null && !state.hidden[0]) points.push({ x: P(r.x, r.start)[0], y: P(r.x, r.start)[1], r: PT.r + 2 });
              if (r.end != null && !state.hidden[1]) points.push({ x: P(r.x, r.end)[0], y: P(r.x, r.end)[1], r: PT.r + 2 });
              // Riktningspilen (början, mitten, spetsen) är också ett hinder, annars dras etiketten över den
              if (spec.arrows.show && !state.hidden[0] && !state.hidden[1] && r.end != null && r.change != null &&
                  Math.abs(r.change) > spec.arrows.threshold) {
                [0, 0.5, 1].forEach(function (k) {
                  var a = P(r.x, r.end + Math.sign(r.change) * arrowOff * k);
                  points.push({ x: a[0], y: a[1], r: 4 });
                });
              }
            });
            vgrD3.relaxLabels(items, rect, { sep: LB.both ? pt(4) : pt(2), direction: "both", points: points,
                                             maxOverlaps: LB.both ? 30 : 20 });
            vgrD3.leaderLines(labG, items, { color: "#999999", width: 1 });
          }
          nodes.place();
          var delay = state.first ? (spec.animation.draw ? dur : 0) : t;
          if (delay) labG.attr("opacity", 0).transition().delay(delay).duration(250).attr("opacity", 1);
        }
      });

      if (capH) vgrD3.textBlock(dynG, T.caption, { x: panelRight, y: box.y1 - capH, size: T.caption_size, font: font, anchor: "end" });
      state.first = false;
    }

    // -------------------------------------------------------------------------
    //  Tooltip
    // -------------------------------------------------------------------------
    function hoverRow(r, event, facet, hor, c, unit, rect) {
      hoverG.selectAll("*").remove();
      hoverG.append("rect")
        .attr("x", hor ? rect.x0 : c - 0.5 * unit).attr("y", hor ? c - 0.5 * unit : rect.y0)
        .attr("width", hor ? rect.x1 - rect.x0 : unit).attr("height", hor ? unit : rect.y1 - rect.y0)
        .attr("fill", "#37474F").attr("fill-opacity", 0.07);
      if (!TT.show) return;
      var html = "<div class='vgr-tt-title'>" + esc(r.x) + "</div>";
      if (hasFacets) html += "<div class='vgr-tt-sub'>" + esc(facet) + "</div>";
      // Dolda perioder (legendklick) visas inte; förändringen kräver båda
      if (!state.hidden[0]) html += vgrD3.tooltipRow(spec.periods[0], r.start == null ? "–" : fmtTip(r.start), spec.colors[0]);
      if (!state.hidden[1]) html += vgrD3.tooltipRow(spec.periods[1], r.end == null ? "–" : fmtTip(r.end), spec.colors[1]);
      if (r.change != null && !state.hidden[0] && !state.hidden[1]) {
        var sign = r.change > 0 ? "+" : "";
        var txt = sign + fmtTip(r.change);
        if (TT.percent && r.start) txt += " (" + sign + vgrD3.fmt(100 * r.change / Math.abs(r.start), TT.set_decimals != null ? TT.set_decimals : 1, " ") + " %)";
        html += vgrD3.tooltipRow("Förändring", txt);
      }
      // tooltip_columns: [start, slut] per kolumn — en rad om perioderna har samma värde, annars en per period
      (TT.columns || []).forEach(function (label, j) {
        var v = r.tt ? r.tt[j] : null;
        if (!v) return;
        var vis = [0, 1].filter(function (k) { return !state.hidden[k] && v[k] != null; });
        if (vis.length === 2 && v[0] === v[1]) html += vgrD3.tooltipRow(label, v[0]);
        else vis.forEach(function (k) { html += vgrD3.tooltipRow(label + " (" + spec.periods[k] + ")", v[k]); });
      });
      tip.show(html, event);
    }

    // -------------------------------------------------------------------------
    //  Kontroller
    // -------------------------------------------------------------------------
    if (IA.sort) {
      var opts = [
        { v: "original|desc", t: "Ursprunglig ordning" },
        { v: "end_value|desc", t: "Slutvärde, fallande" }, { v: "end_value|asc", t: "Slutvärde, stigande" },
        { v: "start_value|desc", t: "Startvärde, fallande" }, { v: "start_value|asc", t: "Startvärde, stigande" },
        { v: "change|desc", t: "Förändring, fallande" }, { v: "change|asc", t: "Förändring, stigande" },
        { v: "range|desc", t: "Störst skillnad" }, { v: "range|asc", t: "Minst skillnad" },
        { v: "alphabetical|asc", t: "Namn A–Ö" }, { v: "alphabetical|desc", t: "Namn Ö–A" }
      ];
      if (!opts.some(function (o) { return o.v === state.sort; })) {
        if (spec.sort.by === "original") state.sort = "original|desc";
        else opts.unshift({ v: state.sort, t: "Startordning" });
      }
      var sel = controls.append("select").attr("aria-label", "Sortera")
        .style("font", "inherit").style("padding", "3px 6px").style("border", "1px solid #b0bec5").style("border-radius", "4px")
        .on("change", function () {
          state.sort = this.value;
          var parts = this.value.split("|");
          state.order = sortOrder(parts[0], parts[1]);
          update(true);
        });
      sel.selectAll("option").data(opts).join("option").attr("value", function (o) { return o.v; }).text(function (o) { return o.t; });
      sel.property("value", state.sort);
    }
    controls.append("span").attr("class", "vgr-spacer");
    if (IA.export) vgrD3.exportButtons(controls, svg.node(), fileName);
    if (controls.selectAll("button,select").empty()) controls.style("display", "none");

    update(false);
  };
})();
