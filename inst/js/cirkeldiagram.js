/* vgr_D3 — cirkel-/munkdiagram (func_d3_cirkeldiagram).
 *
 * En tårta per facett. Bitarna kopplas med sitt namn så att filterbyte, dolda bitar och
 * sortering animeras med en vinkelinterpolering (arc tween). Etiketter ligger i biten när den
 * är stor nog, annars utanför med en knäckt linje; utanförliggande etiketter sprids per sida.
 */
(function () {
  "use strict";
  var vgrD3 = window.vgrD3;
  function pt(v) { return v * 96 / 72; }
  var TAU = Math.PI * 2;

  vgrD3.renderers.cirkeldiagram = function (el, spec) {
    var W = spec.width, H = spec.height, font = spec.font;
    var T = spec.text, F = spec.facets, L = spec.legend, LB = spec.labels, TT = spec.tooltip;
    var IA = spec.interactive, PIE = spec.pie;
    var dur = spec.animation.duration || 0;
    var esc = vgrD3.esc;
    var fileName = vgrD3.slug((T.title && T.title.join(" ")) || "cirkeldiagram");

    var root = d3.select(el).style("max-width", W + "px");
    var controls = vgrD3.controls(el, font);
    var svg = root.append("svg").attr("class", "vgr-d3-svg")
      .attr("width", W).attr("height", H).attr("viewBox", "0 0 " + W + " " + H)
      .attr("role", "img").attr("aria-label", (T.title && T.title.join(" ")) || "Cirkeldiagram")
      .attr("font-family", font)
      .style("width", "100%").style("height", "auto").style("display", "block");
    var tip = vgrD3.tooltip(el, font);

    if (spec.empty) {
      svg.append("text").attr("x", W / 2).attr("y", H / 2).attr("text-anchor", "middle").style("font-size", (18 * (spec.text_scale || 1)) + "px").text("Inga data att visa");
      return;
    }

    var sliceByKey = {}, sliceIndex = {};
    spec.slices.forEach(function (s, i) { sliceByKey[s.key] = s; sliceIndex[s.key] = i; });
    var hasFacets = !(F.levels.length === 1 && F.levels[0] === "__alla");
    var state = { filter: spec.filter.default, hidden: {}, hover: null, first: true };

    function fmtVal(v, dec) { return vgrD3.fmt(v, dec, " "); }
    function fmtPct(share, dec) { return vgrD3.fmt(share * 100, dec, null) + " %"; }
    function labelText(d, share) {
      var dec = LB.decimals, v = d.data.v, name = d.data.x, t;
      switch (LB.format) {
        case "value_only": t = fmtVal(v, dec); break;
        case "value_percent": t = fmtVal(v, dec) + " (" + fmtPct(share, dec) + ")"; break;
        case "x_y": t = name + LB.separator + fmtVal(v, dec); break;
        case "x_percent": t = name + LB.separator + fmtPct(share, dec); break;
        default: t = fmtPct(share, dec);
      }
      if (LB.show_group && LB.format !== "x_y" && LB.format !== "x_percent") t = name + LB.separator + t;
      return t;
    }
    function contrast(color) { return d3.hsl(color).l > 0.62 ? "#000000" : "#ffffff"; }

    function rowsFor(f) {
      var rows = spec.rows.filter(function (r) { return r.k === state.filter && r.f === f && !state.hidden[r.x] && r.v != null; });
      if (PIE.sort === "desc") rows.sort(function (a, b) { return b.v - a.v || sliceIndex[a.x] - sliceIndex[b.x]; });
      else if (PIE.sort === "asc") rows.sort(function (a, b) { return a.v - b.v || sliceIndex[a.x] - sliceIndex[b.x]; });
      else if (PIE.sort === "name") rows.sort(function (a, b) { return a.x.localeCompare(b.x, "sv"); });
      else rows.sort(function (a, b) { return sliceIndex[a.x] - sliceIndex[b.x]; });
      return rows;
    }

    // -------------------------------------------------------------------------
    //  Fasta lager och rubriker
    // -------------------------------------------------------------------------
    svg.append("rect").attr("width", W).attr("height", H).attr("fill", "#ffffff");
    var box = { x0: pt(10), y0: pt(15), x1: W - pt(10), y1: H - pt(10) };
    var headerH = 0;
    var th = vgrD3.textBlock(svg, T.title, { x: box.x0, y: box.y0, size: T.title_size, font: font, anchor: "start", lineHeight: 1.1 });
    headerH += th + (th ? pt(5) : 0);
    var sh = vgrD3.textBlock(svg, T.subtitle, { x: box.x0, y: box.y0 + headerH, size: T.subtitle_size, font: font, anchor: "start" });
    headerH += sh + (sh ? pt(5) : 0);
    if (th || sh) headerH += pt(5);
    var capH = T.caption.length * T.caption_size * 1.2;
    if (capH) vgrD3.textBlock(svg, T.caption, { x: box.x1, y: box.y1 - capH, size: T.caption_size, font: font, anchor: "end" });
    var legendG = svg.append("g");
    var panelsG = svg.append("g");

    // -------------------------------------------------------------------------
    //  Legend (klicka = dölj/visa, hovra = markera)
    // -------------------------------------------------------------------------
    function drawLegend(area) {
      legendG.selectAll("*").remove();
      if (L.position === "none") return;
      var present = {};
      spec.rows.forEach(function (r) { if (r.k === state.filter) present[r.x] = true; });
      var entries = spec.slices.filter(function (s) { return present[s.key]; });
      var key = pt(17.28), rowH = Math.max(L.text_size * 1.3, key + 2);
      var vertical = L.position === "left" || L.position === "right";
      entries = entries.map(function (s) {
        return { s: s, w: key + 6 + vgrD3.measureText(svg, s.key, L.text_size, font, s.bold ? "bold" : null) };
      });
      var titleW = L.title ? vgrD3.measureText(svg, L.title, L.title_size, font) : 0;
      var titleH = L.title && vertical ? L.title_size * 1.6 : 0;
      var lw, lh;
      if (vertical) {
        entries.forEach(function (e, i) { e.x = 0; e.y = titleH + i * rowH; });
        lw = Math.max(titleW, d3.max(entries, function (e) { return e.w; }) || 0) + 8;
        lh = titleH + entries.length * rowH;
      } else {
        var startX = L.title ? titleW + 10 : 0, avail = area.x1 - area.x0, x = startX, y = 0, maxX = 0;
        entries.forEach(function (e) {
          if (x > startX && x + e.w > avail) { x = startX; y += rowH; }
          e.x = x; e.y = y; x += e.w + 16; maxX = Math.max(maxX, x - 16);
        });
        lw = maxX; lh = y + rowH;
      }
      var lx, ly, gap = pt(11);
      if (L.position === "right") { lx = area.x1 - lw; ly = (area.y0 + area.y1) / 2 - lh / 2; area.x1 -= lw + gap; }
      else if (L.position === "left") { lx = area.x0; ly = (area.y0 + area.y1) / 2 - lh / 2; area.x0 += lw + gap; }
      else if (L.position === "top") { lx = (area.x0 + area.x1) / 2 - lw / 2; ly = area.y0; area.y0 += lh + gap; }
      else { lx = (area.x0 + area.x1) / 2 - lw / 2; ly = area.y1 - lh; area.y1 -= lh + gap; }
      var lg = legendG.append("g").attr("transform", "translate(" + lx + "," + ly + ")");
      if (L.title) {
        lg.append("text").attr("y", vertical ? L.title_size : rowH / 2).attr("dy", vertical ? null : "0.35em")
          .style("font-size", L.title_size + "px").style("font-family", font).text(L.title);
      }
      var item = lg.selectAll("g").data(entries).join("g")
        .attr("transform", function (e) { return "translate(" + e.x + "," + e.y + ")"; })
        .attr("opacity", function (e) { return state.hidden[e.s.key] ? 0.35 : 1; })
        .style("cursor", IA.legend ? "pointer" : null);
      item.append("rect").attr("width", function (e) { return e.w; }).attr("height", rowH).attr("fill", "#ffffff").attr("fill-opacity", 0);
      item.append("rect").attr("y", (rowH - key) / 2).attr("width", key).attr("height", key)
        .attr("fill", function (e) { return e.s.color; }).attr("fill-opacity", function (e) { return e.s.opacity; });
      item.append("text").attr("x", key + 6).attr("y", rowH / 2).attr("dy", "0.35em")
        .style("font-size", L.text_size + "px").style("font-family", font)
        .style("font-weight", function (e) { return e.s.bold ? "bold" : null; })
        .text(function (e) { return e.s.key; });
      if (IA.legend) {
        item.append("title").text("Klicka för att dölja/visa");
        item.on("mouseenter", function (event, e) { setHover(e.s.key); })
          .on("mouseleave", function () { setHover(null); })
          .on("click", function (event, e) {
            if (state.hidden[e.s.key]) delete state.hidden[e.s.key]; else state.hidden[e.s.key] = true;
            update(true);
          });
      }
    }

    function setHover(key) {
      state.hover = key;
      panelsG.selectAll("path.vgr-slice").attr("opacity", function (d) {
        var base = sliceByKey[d.data.x].opacity;
        return key == null || d.data.x === key ? base : base * 0.3;
      });
    }

    // -------------------------------------------------------------------------
    //  Uppdatering
    // -------------------------------------------------------------------------
    function update(animate) {
      var t = animate && dur > 0 ? dur : 0;
      tip.hide();
      var area = { x0: box.x0, y0: box.y0 + headerH, x1: box.x1, y1: box.y1 - (capH ? capH + pt(5.5) : 0) };
      drawLegend(area);

      var n = F.levels.length, ncol = Math.min(F.ncol, n), nrow = Math.ceil(n / ncol);
      var sp = hasFacets ? pt(12) : 0;
      var stripH = hasFacets ? d3.max(F.levels, function (f) { return String(f).split("\n").length; }) * T.facet_title_size * 1.1 + pt(8) : 0;
      var cellW = (area.x1 - area.x0 - (ncol - 1) * sp) / ncol;
      var cellH = (area.y1 - area.y0 - (nrow - 1) * sp) / nrow;
      var hasOutside = LB.show && LB.position !== "inside";

      var panels = panelsG.selectAll("g.vgr-panel").data(F.levels, function (f) { return f; })
        .join(function (enter) {
          var g = enter.append("g").attr("class", "vgr-panel");
          g.append("g").attr("class", "vgr-strip");
          var c = g.append("g").attr("class", "vgr-center");
          c.append("g").attr("class", "vgr-slices");
          c.append("g").attr("class", "vgr-lines");
          c.append("g").attr("class", "vgr-labels");
          c.append("g").attr("class", "vgr-mid");
          return g;
        });

      panels.each(function (f, i) {
        var g = d3.select(this);
        var col = i % ncol, row = Math.floor(i / ncol);
        var cx = area.x0 + col * (cellW + sp), cy = area.y0 + row * (cellH + sp);
        var pw = cellW, ph = cellH - stripH;
        var mx = cx + pw / 2, my = cy + stripH + ph / 2;

        var strip = g.select(".vgr-strip");
        strip.selectAll("*").remove();
        if (hasFacets) vgrD3.textBlock(strip, String(f).split("\n"), { x: mx, y: cy, size: T.facet_title_size, font: font, lineHeight: 1.1 });

        var rows = rowsFor(f);
        var total = d3.sum(rows, function (r) { return r.v; });
        var center = g.select(".vgr-center");
        center.attr("transform", "translate(" + mx + "," + my + ")");

        var start = PIE.start * Math.PI / 180;
        var pie = d3.pie().value(function (d) { return d.v; }).sort(null)
          .startAngle(start).endAngle(start + TAU).padAngle(PIE.pad * Math.PI / 180);
        var arcs = pie(rows);

        // Vilka etiketter som får plats i sin bit beror på radien — och radien på hur mycket plats
        // etiketterna utanför behöver. Två varv räcker: först full radie, sedan krympt för de utanför.
        var maxExplode = d3.max(spec.slices, function (s) { return s.highlighted ? PIE.explode : 0; }) || 0;
        function planLabels(rad) {
          var plan = { inside: [], outside: [] };
          if (!LB.show || !total) return plan;
          arcs.forEach(function (d) {
            var src = spec.rows.find(function (rw) { return rw.k === state.filter && rw.f === f && rw.x === d.data.x; });
            if (!src || !src.lab || d.data.v <= 0) return;
            if (LB.min_value != null && d.data.v < LB.min_value) return;
            var share = d.data.v / total, text = labelText(d, share);
            var w = vgrD3.measureText(svg, text, LB.size, font, sliceByKey[d.data.x].bold ? "bold" : null) + 6;
            var lr = PIE.inner > 0 ? rad * (1 + PIE.inner) / 2 : rad * 0.62;
            var chord = 2 * lr * Math.sin(Math.min(Math.PI / 2, (d.endAngle - d.startAngle) / 2));
            var ring = PIE.inner > 0 ? rad * (1 - PIE.inner) : rad;
            var fits = chord >= w && ring >= LB.size * 1.3 && w <= (PIE.inner > 0 ? ring * 1.9 : rad * 0.9);
            var inside = LB.position === "inside" || (LB.position === "auto" && share >= LB.min_share && fits);
            (inside ? plan.inside : plan.outside).push({ d: d, share: share, text: text, tw: w, lr: lr });
          });
          return plan;
        }
        var r = Math.max(10, Math.min(pw, ph) / 2 / (1 + maxExplode + 0.04) * 0.96);
        var plan = planLabels(r);
        if (plan.outside.length) {
          var wOut = d3.max(plan.outside, function (o) { return o.tw; });
          r = Math.max(10, Math.min(r, (pw / 2 - wOut - 16) / (1.18 + maxExplode), (ph / 2 - 12) / (1.12 + maxExplode)));
          plan = planLabels(r);
        }
        var arc = d3.arc().innerRadius(r * PIE.inner).outerRadius(r).cornerRadius(PIE.corner);
        function offset(d, extra) {
          var e = (sliceByKey[d.data.x].highlighted ? PIE.explode : 0) + (extra || 0);
          if (!e) return "translate(0,0)";
          var a = (d.startAngle + d.endAngle) / 2;
          return "translate(" + (Math.sin(a) * r * e) + "," + (-Math.cos(a) * r * e) + ")";
        }

        // Bitar med vinkelinterpolering från förra läget
        var sel = center.select(".vgr-slices").selectAll("path.vgr-slice").data(arcs, function (d) { return d.data.x; });
        sel.exit()
          .transition().duration(t).attrTween("d", function (d) {
            var i2 = d3.interpolate(this.__cur || d, { startAngle: d.startAngle, endAngle: d.startAngle, padAngle: 0 });
            return function (tt) { return arc(i2(tt)); };
          }).remove();
        var ent = sel.enter().append("path").attr("class", "vgr-slice")
          .each(function (d) {
            // Nya bitar växer ut från sin startvinkel (vid öppning: alla från startpunkten)
            var a0 = state.first ? start : d.startAngle;
            this.__cur = { startAngle: a0, endAngle: a0, padAngle: d.padAngle };
          });
        var all = ent.merge(sel)
          .attr("fill", function (d) { return sliceByKey[d.data.x].color; })
          .attr("stroke", PIE.border_color).attr("stroke-width", PIE.border_size).attr("stroke-linejoin", "round")
          .attr("opacity", function (d) { return sliceByKey[d.data.x].opacity; })
          .on("mouseenter", function (event, d) {
            d3.select(this).attr("transform", offset(d, 0.03));
            showTip(event, d, total, f);
          })
          .on("mousemove", function (event, d) { showTip(event, d, total, f); })
          .on("mouseleave", function (event, d) { d3.select(this).attr("transform", offset(d)); tip.hide(); });
        var animOpen = state.first && spec.animation.draw && dur > 0;
        var tdur = animOpen ? dur : t;
        if (tdur) {
          all.transition().duration(tdur).ease(animOpen ? d3.easeCubicOut : d3.easeCubicInOut)
            .attr("transform", function (d) { return offset(d); })
            .attrTween("d", function (d) {
              var i2 = d3.interpolate(this.__cur || d, d);
              this.__cur = i2(1);
              return function (tt) { return arc(i2(tt)); };
            });
        } else {
          all.attr("transform", function (d) { return offset(d); }).attr("d", arc).each(function (d) { this.__cur = d; });
        }

        // Text i mitten (munkdiagram)
        var mid = center.select(".vgr-mid");
        mid.selectAll("*").remove();
        if (PIE.center_text != null) {
          var ctext = PIE.center_text === "total" ? fmtVal(total, 0) : PIE.center_text;
          // Textstorlek som får plats i hålet (bredd ≈ 0,6 × storlek per tecken)
          var hole = PIE.inner > 0 ? 2 * r * PIE.inner * 0.8 : r;
          // Mitttexten anpassas till hålet (annars flödar den över); text_scale skalar minimistorleken
          var cts = spec.text_scale || 1;
          var csize = Math.max(11 * cts, Math.min(r * 0.24, hole / Math.max(1, String(ctext).length * 0.62)));
          mid.append("text").attr("text-anchor", "middle").attr("dy", PIE.center_label ? "0.1em" : "0.35em")
            .attr("fill", "#263238").style("font-size", csize + "px").style("font-weight", "bold").style("font-family", font).text(ctext);
          if (PIE.center_label) {
            mid.append("text").attr("text-anchor", "middle").attr("y", csize * 0.35).attr("dy", "1em")
              .attr("fill", "#546e7a").style("font-size", Math.max(10 * cts, csize * 0.42) + "px").style("font-family", font).text(PIE.center_label);
          }
        }
        if (!rows.length || total === 0) {
          mid.append("text").attr("text-anchor", "middle").attr("dy", "0.35em").attr("fill", "#78909c")
            .style("font-size", (14 * (spec.text_scale || 1)) + "px").style("font-family", font).text("Inga värden");
        }

        // Etiketter
        var labG = center.select(".vgr-labels"), lineG = center.select(".vgr-lines");
        labG.selectAll("*").remove();
        lineG.selectAll("*").remove();
        if (!LB.show || !total) return;
        var inside = [], outside = [];
        plan.inside.concat(plan.outside).forEach(function (pl) {
          var d = pl.d;
          var isInside = plan.inside.indexOf(pl) >= 0;
          var a = (d.startAngle + d.endAngle) / 2;
          var s = sliceByKey[d.data.x];
          var ex = s.highlighted ? PIE.explode * r : 0;
          var item = { d: d, a: a, text: pl.text, bold: s.bold };
          if (isInside) {
            item.x = Math.sin(a) * (pl.lr + ex); item.y = -Math.cos(a) * (pl.lr + ex);
            item.color = LB.color === "auto" ? contrast(s.color) : LB.color;
            inside.push(item);
          } else {
            item.side = Math.sin(a) >= 0 ? 1 : -1;
            item.ox = Math.sin(a) * (r + ex); item.oy = -Math.cos(a) * (r + ex);
            item.ex = Math.sin(a) * (r * 1.1 + ex); item.ey = -Math.cos(a) * (r * 1.1 + ex);
            item.y = item.ey;
            item.color = LB.color === "auto" ? "#263238" : LB.color;
            outside.push(item);
          }
        });
        var opts = { size: LB.size, font: font, box: LB.box, box_color: LB.box_color, box_alpha: LB.box_alpha,
                     glow: LB.glow, glow_color: LB.glow_color, glow_width: LB.glow_width };
        var inNodes = vgrD3.labelNodes(labG, inside, opts);
        inNodes.place();
        var outNodes = vgrD3.labelNodes(labG, outside, opts);
        // Utanför: sprid per sida uppifrån och ned, vänster-/högerjustera vid kolumnen
        [1, -1].forEach(function (side) {
          var list = outside.filter(function (o) { return o.side === side; }).sort(function (a, b) { return a.ey - b.ey; });
          if (LB.repel) {
            var yMin = -ph / 2 + 4, yCur = yMin;
            list.forEach(function (o) { o.y = Math.max(o.ey, yCur + o.h / 2); yCur = o.y + o.h / 2 + 2; });
            var yMax = ph / 2 - 4;
            for (var k = list.length - 1; k >= 0; k--) {
              if (list[k].y + list[k].h / 2 > yMax) list[k].y = yMax - list[k].h / 2;
              yMax = list[k].y - list[k].h / 2 - 2;
            }
          }
          list.forEach(function (o) {
            var colX = side * (r * 1.18 + (sliceByKey[o.d.data.x].highlighted ? PIE.explode * r : 0));
            o.x = colX + side * (o.w / 2 + 4);
            lineG.append("polyline")
              .attr("points", [[o.ox, o.oy], [o.ex, o.ey], [colX, o.y]].map(function (p) { return p.join(","); }).join(" "))
              .attr("fill", "none").attr("stroke", "#90a4ae").attr("stroke-width", 1);
          });
        });
        outNodes.place();
        var delay = state.first ? (spec.animation.draw ? dur : 0) : t;
        [labG, lineG].forEach(function (gg) {
          if (delay) gg.attr("opacity", 0).transition().delay(delay).duration(250).attr("opacity", 1);
          else gg.attr("opacity", 1);
        });
      });
      refreshControls();
      state.first = false;
    }

    function showTip(event, d, total, facet) {
      if (!TT.show) return;
      var s = sliceByKey[d.data.x];
      var html = "<div class='vgr-tt-title'>" + esc(d.data.x) + "</div>";
      var sub = [];
      if (hasFacets) sub.push(facet);
      if (spec.filter.enabled) sub.push(state.filter);
      if (sub.length) html += "<div class='vgr-tt-sub'>" + esc(sub.join(" · ")) + "</div>";
      html += vgrD3.tooltipRow("Värde", fmtVal(d.data.v, TT.decimals) + TT.unit, s.color);
      html += vgrD3.tooltipRow("Andel", fmtPct(total ? d.data.v / total : 0, TT.set_decimals != null ? TT.set_decimals : 1));
      html += vgrD3.tooltipRow("Totalt", fmtVal(total, TT.decimals) + TT.unit);
      tip.show(html, event);
    }

    // -------------------------------------------------------------------------
    //  Kontroller
    // -------------------------------------------------------------------------
    var timer = null, playBtn = null, filterSel = null;
    function stopPlay() { if (timer) { timer.stop(); timer = null; } if (playBtn) playBtn.text("▶"); }
    function setFilter(k) { state.filter = k; if (filterSel) filterSel.property("value", k); update(true); }
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
    var dynamic = controls.append("span").style("display", "inline-flex").style("gap", "8px");
    controls.append("span").attr("class", "vgr-spacer");
    if (IA.export) vgrD3.exportButtons(controls, svg.node(), fileName);
    function refreshControls() {
      dynamic.selectAll("*").remove();
      if (Object.keys(state.hidden).length) vgrD3.button(dynamic, "Visa alla", function () { state.hidden = {}; update(true); });
    }
    if (controls.selectAll("button,select").empty()) controls.style("display", "none");

    update(false);
  };
})();
