/* vgr_D3 — täthetskarta (func_d3_karta_heatmap).
 *
 * Konturpolygonerna kommer färdiga från R (ggplot2:s stat_density_2d) i kartans koordinater,
 * en uppsättning per variant "period|grupp|utjämning". Här ritas bakgrund, extra lager,
 * konturer (fyllda polygoner med genomskinlighet, som geom = "polygon") och punkter.
 */
(function () {
  "use strict";
  var vgrD3 = window.vgrD3;

  vgrD3.renderers.karta_heatmap = function (el, spec) {
    var W = spec.width, H = spec.height, font = spec.font;
    var T = spec.text, HEAT = spec.heat, PTS = spec.points, L = spec.legend, TT = spec.tooltip, IA = spec.interactive;
    var dur = spec.animation.duration || 0;
    var esc = vgrD3.esc;
    var fileName = vgrD3.slug((T.title && T.title.join(" ")) || "tathetskarta");

    var root = d3.select(el).style("width", W + "px");
    var controls = vgrD3.controls(el, font);
    var svg = root.append("svg").attr("class", "vgr-d3-svg")
      .attr("width", W).attr("height", H).attr("viewBox", "0 0 " + W + " " + H)
      .attr("role", "img").attr("aria-label", (T.title && T.title.join(" ")) || "Täthetskarta")
      .attr("font-family", font).style("display", "block");
    var tip = vgrD3.tooltip(el, font);

    var state = {
      period: spec.default_period, dgroup: "__alla", adjustIdx: HEAT.adjust_index,
      showHeat: true, showPoints: PTS.show, hidden: {}, transform: d3.zoomIdentity
    };
    // Utjämningens nyckeltext kommer färdig från R (1/3 skrivs olika i R och JavaScript)
    function variantKey() { return state.period + "|" + state.dgroup + "|" + HEAT.adjust_keys[state.adjustIdx]; }
    PTS.data.forEach(function (d, i) { d._i = i; });   // fast nyckel per punkt, oberoende av filtrering
    var heatColor = d3.scaleLinear().domain(HEAT.domain).range([HEAT.low, HEAT.high]).interpolate(d3.interpolateLab);
    function groupColor(g) { return PTS.group_colors && g != null ? PTS.group_colors[g] : PTS.color; }

    // -------------------------------------------------------------------------
    //  Layout: rubriker, legender, kartyta
    // -------------------------------------------------------------------------
    var m = spec.margin;
    var box = { x0: m.l, y0: m.t, x1: W - m.r, y1: H - m.b };
    svg.append("rect").attr("width", W).attr("height", H).attr("fill", "#ffffff");
    var y = box.y0 + (T.title.length ? T.title_margin_top : 0);
    var titleH = T.title.length * T.title_size * 1.2;
    var subTop = y + titleH + (titleH ? 4 : 0);
    var subH = T.subtitle.length * T.subtitle_size * 1.25;
    var top = subTop + subH + (titleH || subH ? 8 : 0);
    var capH = T.caption.length * T.caption_size * 1.2;
    var bottom = box.y1 - capH - (capH ? 6 : 0);

    // Legendblock: färgskala för tätheten (om titel eller position anges) och grupper för punkterna
    var cm = 96 / 2.54, gap = 0.5 * cm;
    var vertical = L.position === "left" || L.position === "right";
    var blocks = [];
    if (L.position !== "none") {
      var tickW = d3.max(HEAT.tick_labels, function (t) { return vgrD3.measureText(svg, t, L.text_size, font); }) || 0;
      var titleW = L.title ? vgrD3.measureText(svg, L.title, L.text_size, font) : 0;
      var barW = vertical ? 1.5 * cm : 7.5 * cm, barH = vertical ? 2.5 * cm : 0.5 * cm;
      blocks.push({
        kind: "bar", barW: barW, barH: barH,
        w: vertical ? Math.max(titleW, barW + 6 + tickW) : titleW + (titleW ? 10 : 0) + barW + tickW / 2,
        h: vertical ? (L.title ? L.text_size * 1.6 : 0) + barH : Math.max(barH + L.text_size * 1.5, L.text_size * 1.4)
      });
      if (PTS.groups.length && PTS.show) {
        var keyD = L.key_shape === "circle" ? 0.6 * cm : 0.45 * cm, rowH = Math.max(keyD + 4, L.text_size * 1.3);
        var gw = PTS.groups.map(function (g) { return keyD + 6 + vgrD3.measureText(svg, g, L.text_size, font); });
        var gtW = L.group_title ? vgrD3.measureText(svg, L.group_title, L.text_size, font) : 0;
        blocks.push({
          kind: "groups", keyD: keyD, rowH: rowH, gw: gw,
          w: vertical ? Math.max(gtW, d3.max(gw)) : gtW + 10 + d3.sum(gw) + 16 * gw.length,
          h: vertical ? (L.group_title ? L.text_size * 1.6 : 0) + PTS.groups.length * rowH : rowH
        });
      }
    }
    // Vågräta block läggs under varandra om de inte ryms bredvid varandra
    var sideBySide = !vertical && d3.sum(blocks, function (b) { return b.w; }) + gap * Math.max(0, blocks.length - 1) <= box.x1 - box.x0;
    var legW = sideBySide ? d3.sum(blocks, function (b) { return b.w; }) + gap * Math.max(0, blocks.length - 1) : (d3.max(blocks, function (b) { return b.w; }) || 0);
    var legH = sideBySide ? (d3.max(blocks, function (b) { return b.h; }) || 0) : d3.sum(blocks, function (b) { return b.h; }) + gap * Math.max(0, blocks.length - 1);

    var ext = { x0: box.x0, y0: top, x1: box.x1, y1: bottom };
    if (blocks.length) {
      if (L.position === "right") ext.x1 -= legW + gap;
      if (L.position === "left") ext.x0 += legW + gap;
      if (L.position === "top") ext.y0 += legH + gap;
      if (L.position === "bottom") ext.y1 -= legH + gap;
    }

    // Kartans utsnitt = basemaps bbox med marginal (coord_sf(xlim, ylim, expand = FALSE))
    var bb = spec.bbox;
    var bboxFeature = { type: "Feature", geometry: { type: "Polygon", coordinates: [[[bb[0], bb[1]], [bb[2], bb[1]], [bb[2], bb[3]], [bb[0], bb[3]], [bb[0], bb[1]]]] } };
    var proj = d3.geoIdentity().reflectY(true).fitExtent([[ext.x0, ext.y0], [ext.x1, ext.y1]], bboxFeature);
    var path = d3.geoPath(proj);
    var p0 = proj([bb[0], bb[3]]), p1 = proj([bb[2], bb[1]]);
    var view = { x0: p0[0], y0: p0[1], x1: p1[0], y1: p1[1] };

    // Rubriker (centrerade som theme_void + hjust 0.5)
    var titleX = T.title_position === "panel" ? (view.x0 + view.x1) / 2 : (box.x0 + box.x1) / 2;
    vgrD3.textBlock(svg, T.title, { x: titleX, y: y, size: T.title_size, font: font });
    vgrD3.textBlock(svg, T.subtitle, { x: titleX, y: subTop, size: T.subtitle_size, font: font, lineHeight: 1.25 });
    vgrD3.textBlock(svg, T.caption, { x: (box.x0 + box.x1) / 2, y: box.y1 - capH, size: T.caption_size, font: font });

    // -------------------------------------------------------------------------
    //  Kartlager
    // -------------------------------------------------------------------------
    var clipId = el.id + "-clip";
    svg.append("defs").append("clipPath").attr("id", clipId).append("rect")
      .attr("x", view.x0).attr("y", view.y0).attr("width", view.x1 - view.x0).attr("height", view.y1 - view.y0);
    var mapArea = svg.append("g").attr("clip-path", "url(#" + clipId + ")");
    mapArea.append("rect").attr("x", view.x0).attr("y", view.y0).attr("width", view.x1 - view.x0).attr("height", view.y1 - view.y0)
      .attr("fill", "#ffffff").attr("fill-opacity", 0);
    var zoomG = mapArea.append("g");
    function drawLayer(fc, fill, stroke, width) {
      if (!fc) return;
      zoomG.append("g").selectAll("path").data(fc.features).join("path").attr("d", path)
        .attr("fill", fill).attr("stroke", stroke).attr("stroke-width", width)
        .attr("vector-effect", "non-scaling-stroke").attr("pointer-events", "none");
    }
    drawLayer(spec.basemap, spec.basemap_color, spec.basemap_color_line, 1.42);
    drawLayer(spec.layers.road, "#5e5e5e", "#5e5e5e", 1.42);
    drawLayer(spec.layers.ocean, "#b5c7eb", "#b5c7eb", 1.42);
    drawLayer(spec.layers.lake, "#b5c7eb", "#b5c7eb", 1.42);
    var heatG = zoomG.append("g").attr("pointer-events", "none");
    var namesG = mapArea.append("g").attr("pointer-events", "none");
    var pointsG = mapArea.append("g");
    var yearText = svg.append("text").attr("x", view.x0 + 6).attr("y", view.y0 + 38)
      .attr("fill", "#37474F").attr("fill-opacity", 0.45).style("font-size", (36 * (spec.text_scale || 1)) + "px").style("font-weight", "bold").style("font-family", font);
    var legendG = svg.append("g");

    namesG.selectAll("text").data(spec.layers.names).join("text")
      .attr("text-anchor", "middle").attr("dy", "0.35em").attr("fill", "#000000")
      .style("font-size", spec.layers.names_size + "px").style("font-family", font).text(function (d) { return d.text; });

    function contourPath(k) {
      return "M" + k.pts.map(function (q) { return proj(q).join(","); }).join("L") + "Z";
    }
    function drawHeat(animate) {
      var old = heatG.selectAll("g.vgr-variant");
      (animate && dur ? old.transition().duration(dur).attr("opacity", 0) : old).remove();
      if (!state.showHeat) return;
      var list = HEAT.variants[variantKey()] || [];
      var g = heatG.append("g").attr("class", "vgr-variant").attr("opacity", animate && dur ? 0 : 1);
      g.selectAll("path").data(list).join("path")
        .attr("d", contourPath).attr("fill", function (k) { return heatColor(k.level); })
        .attr("fill-opacity", HEAT.alpha).attr("stroke", "none");
      if (animate && dur) g.transition().duration(dur).attr("opacity", 1);
    }

    function visiblePoints() {
      if (!state.showPoints) return [];
      return PTS.data.filter(function (d) {
        return (!spec.time.enabled || d.t === state.period) && !(d.g != null && state.hidden[d.g]);
      });
    }
    // Nedtoning när en grupp hovras i legenden (state.hoverGroup)
    function pointOpacity(d) { return state.hoverGroup == null || d.g === state.hoverGroup ? null : 0.15; }
    function drawPoints() {
      var sel = pointsG.selectAll("circle").data(visiblePoints(), function (d) { return d._i; });
      sel.exit().remove();
      sel.enter().append("circle")
        .attr("r", PTS.r).attr("fill-opacity", PTS.alpha)
        .style("cursor", TT.show ? "pointer" : null)
        .on("mouseenter", function (event, d) { d3.select(this).attr("r", PTS.r * 1.8).attr("stroke", "#ffffff").attr("stroke-width", 1.5); showTip(event, d); })
        .on("mousemove", function (event) { if (TT.show) tip.move(event); })
        .on("mouseleave", function () { d3.select(this).attr("r", PTS.r).attr("stroke", null); tip.hide(); })
        .merge(sel).attr("fill", function (d) { return groupColor(d.g); }).attr("opacity", pointOpacity);
      positionUnscaled();
    }
    function positionUnscaled() {
      var t = state.transform;
      pointsG.selectAll("circle")
        .attr("cx", function (d) { return t.apply(proj([d.x, d.y]))[0]; })
        .attr("cy", function (d) { return t.apply(proj([d.x, d.y]))[1]; });
      namesG.selectAll("text")
        .attr("x", function (d) { return t.apply(proj([d.x, d.y]))[0]; })
        .attr("y", function (d) { return t.apply(proj([d.x, d.y]))[1]; });
    }

    function showTip(event, d) {
      if (!TT.show) return;
      var html = d.n != null ? "<div class='vgr-tt-title'>" + esc(d.n) + "</div>" : "";
      if (spec.time.enabled) html += "<div class='vgr-tt-sub'>" + esc(d.t) + "</div>";
      html += vgrD3.tooltipRow(TT.value_label, d.v == null ? "–" : d.v);
      if (d.g != null) html += vgrD3.tooltipRow(TT.group_label, d.g, groupColor(d.g));
      (d.c || []).forEach(function (v, i) { if (v != null) html += vgrD3.tooltipRow(TT.column_labels[i], v); });
      tip.show(html, event);
    }

    // -------------------------------------------------------------------------
    //  Legender
    // -------------------------------------------------------------------------
    function drawLegend() {
      legendG.selectAll("*").remove();
      if (!blocks.length) return;
      var lx, ly;
      if (L.position === "right") { lx = box.x1 - legW; ly = (ext.y0 + ext.y1) / 2 - legH / 2; }
      else if (L.position === "left") { lx = box.x0; ly = (ext.y0 + ext.y1) / 2 - legH / 2; }
      else if (L.position === "top") { lx = (box.x0 + box.x1) / 2 - legW / 2; ly = top; }
      else { lx = (box.x0 + box.x1) / 2 - legW / 2; ly = ext.y1 + gap; }
      var cx = lx, cy = ly;
      blocks.forEach(function (b) {
        // Staplade vågräta block centreras var för sig
        var bx = !vertical && !sideBySide ? (box.x0 + box.x1) / 2 - b.w / 2 : cx;
        var g = legendG.append("g").attr("transform", "translate(" + bx + "," + cy + ")");
        if (b.kind === "bar") drawBar(g, b); else drawGroups(g, b);
        if (sideBySide) cx += b.w + gap; else cy += b.h + gap;
      });
    }
    function drawBar(g, b) {
      var gid = el.id + "-grad";
      var grad = g.append("defs").append("linearGradient").attr("id", gid)
        .attr("x1", "0%").attr("x2", vertical ? "0%" : "100%").attr("y1", vertical ? "100%" : "0%").attr("y2", "0%");
      d3.range(0, 1.0001, 0.1).forEach(function (s) {
        grad.append("stop").attr("offset", (s * 100) + "%").attr("stop-color", heatColor(HEAT.domain[0] + s * (HEAT.domain[1] - HEAT.domain[0])));
      });
      var x0 = 0, y0 = 0;
      if (L.title) {
        if (vertical) { g.append("text").attr("y", L.text_size).style("font-size", L.text_size + "px").style("font-family", font).text(L.title); y0 = L.text_size * 1.6; }
        else { var tw = vgrD3.measureText(svg, L.title, L.text_size, font); g.append("text").attr("y", b.barH / 2).attr("dy", "0.35em").style("font-size", L.text_size + "px").style("font-family", font).text(L.title); x0 = tw + 10; }
      }
      g.append("rect").attr("x", x0).attr("y", y0).attr("width", b.barW).attr("height", b.barH).attr("fill", "url(#" + gid + ")");
      var s = vertical ? d3.scaleLinear().domain(HEAT.domain).range([y0 + b.barH, y0]) : d3.scaleLinear().domain(HEAT.domain).range([x0, x0 + b.barW]);
      HEAT.ticks.forEach(function (tv, i) {
        var p = s(tv);
        g.append("line").attr("x1", vertical ? x0 : p).attr("x2", vertical ? x0 + b.barW * 0.2 : p)
          .attr("y1", vertical ? p : y0 + b.barH).attr("y2", vertical ? p : y0 + b.barH * 0.8).attr("stroke", "#ffffff");
        g.append("text").attr("x", vertical ? x0 + b.barW + 6 : p).attr("y", vertical ? p : y0 + b.barH + 4)
          .attr("dy", vertical ? "0.35em" : "0.8em").attr("text-anchor", vertical ? "start" : "middle")
          .style("font-size", L.text_size + "px").style("font-family", font).text(HEAT.tick_labels[i]);
      });
    }
    function drawGroups(g, b) {
      var x = 0, yy = 0;
      if (L.group_title) {
        if (vertical) { g.append("text").attr("y", L.text_size).style("font-size", L.text_size + "px").style("font-family", font).text(L.group_title); yy = L.text_size * 1.6; }
        else { g.append("text").attr("y", b.rowH / 2).attr("dy", "0.35em").style("font-size", L.text_size + "px").style("font-family", font).text(L.group_title); x = vgrD3.measureText(svg, L.group_title, L.text_size, font) + 10; }
      }
      PTS.groups.forEach(function (grp, i) {
        var item = g.append("g").attr("transform", "translate(" + x + "," + yy + ")")
          .attr("opacity", state.hidden[grp] ? 0.35 : 1).style("cursor", IA.legend ? "pointer" : null);
        item.append("rect").attr("width", b.gw[i]).attr("height", b.rowH).attr("fill", "#ffffff").attr("fill-opacity", 0);
        item.append("circle").attr("cx", b.keyD / 2).attr("cy", b.rowH / 2).attr("r", b.keyD / 2)
          .attr("fill", groupColor(grp)).attr("stroke", L.key_shape === "circle" ? "#000000" : null).attr("stroke-width", 0.7);
        item.append("text").attr("x", b.keyD + 6).attr("y", b.rowH / 2).attr("dy", "0.35em")
          .style("font-size", L.text_size + "px").style("font-family", font).text(grp);
        if (IA.legend) {
          item.append("title").text("Klicka för att dölja/visa punkterna");
          item.on("click", function () {
            if (state.hidden[grp]) delete state.hidden[grp]; else state.hidden[grp] = true;
            state.hoverGroup = null;
            drawPoints(); drawLegend();
          })
            // Hovring tonar ned de andra gruppernas punkter (som legenden i övriga diagram)
            .on("mouseenter", function () {
              if (state.hidden[grp]) return;
              state.hoverGroup = grp;
              pointsG.selectAll("circle").attr("opacity", pointOpacity);
            })
            .on("mouseleave", function () { state.hoverGroup = null; pointsG.selectAll("circle").attr("opacity", pointOpacity); });
        }
        if (vertical) yy += b.rowH; else x += b.gw[i] + 16;
      });
    }

    // -------------------------------------------------------------------------
    //  Zoom
    // -------------------------------------------------------------------------
    var zoom = d3.zoom().scaleExtent([1, 30])
      .extent([[view.x0, view.y0], [view.x1, view.y1]])
      .translateExtent([[view.x0, view.y0], [view.x1, view.y1]])
      .on("start", function () { tip.hide(); })
      .on("zoom", function (event) { state.transform = event.transform; zoomG.attr("transform", event.transform); positionUnscaled(); });
    if (IA.zoom) svg.call(zoom).on("dblclick.zoom", null);

    // -------------------------------------------------------------------------
    //  Kontroller
    // -------------------------------------------------------------------------
    function toggleButton(label, key, onChange) {
      var b = vgrD3.button(controls, label, function () {
        state[key] = !state[key];
        b.classed("vgr-active", state[key]);
        onChange();
      });
      b.classed("vgr-active", state[key]);
      return b;
    }
    if (IA.layer_toggle) {
      toggleButton("Täthet", "showHeat", function () { drawHeat(false); });
      if (PTS.data.length) toggleButton("Punkter", "showPoints", drawPoints);
    }
    function selectBox(label, values, textOf, current, onChange) {
      controls.append("span").style("color", "#546e7a").text(label);
      var s = controls.append("select").attr("aria-label", label)
        .style("font", "inherit").style("padding", "3px 6px").style("border", "1px solid #b0bec5").style("border-radius", "4px")
        .on("change", function () { onChange(this.value); });
      s.selectAll("option").data(values).join("option").attr("value", function (v) { return v; }).text(textOf);
      s.property("value", current);
      return s;
    }
    if (IA.group_density) {
      selectBox("Täthet för:", spec.density_groups, function (v) { return v === "__alla" ? "Alla" : v; }, state.dgroup,
        function (v) { state.dgroup = v; drawHeat(true); });
    }
    if (IA.adjust_slider) {
      var adjVals = HEAT.adjust_values;
      controls.append("span").style("color", "#546e7a").text("Utjämning:");
      var adjLabel;
      var adjText = function () { return vgrD3.fmt(adjVals[state.adjustIdx], 2, null).replace(/0+$/, "").replace(/,$/, ""); };
      controls.append("input").attr("type", "range").attr("min", 0).attr("max", adjVals.length - 1).attr("step", 1)
        .attr("aria-label", "Utjämning").property("value", state.adjustIdx)
        .on("input", function () {
          state.adjustIdx = +this.value;
          adjLabel.text(adjText());
          drawHeat(true);
        });
      adjLabel = controls.append("span").attr("class", "vgr-year");
      adjLabel.text(adjText());
    }
    var timer = null, playBtn = null, slider = null, perLabel = null;
    function setPeriod(v) {
      state.period = v;
      if (slider) slider.property("value", spec.periods.indexOf(v));
      if (perLabel) perLabel.text(v);
      yearText.text(spec.time.label_show ? v : "");
      drawHeat(true); drawPoints();
    }
    function stopPlay() { if (timer) { timer.stop(); timer = null; } if (playBtn) playBtn.text("▶"); }
    if (spec.time.enabled) {
      playBtn = vgrD3.button(controls, "▶", function () {
        if (timer) { stopPlay(); return; }
        var vals = spec.periods;
        if (vals.indexOf(state.period) >= vals.length - 1) setPeriod(vals[0]);
        playBtn.text("❚❚");
        timer = d3.interval(function () {
          var i = vals.indexOf(state.period);
          if (i >= vals.length - 1) { stopPlay(); return; }
          setPeriod(vals[i + 1]);
        }, spec.animation.play_interval);
      }, "Spela upp");
      slider = controls.append("input").attr("type", "range").attr("min", 0).attr("max", spec.periods.length - 1).attr("step", 1)
        .attr("aria-label", "Välj period")
        .on("input", function () { stopPlay(); setPeriod(spec.periods[+this.value]); });
      perLabel = controls.append("span").attr("class", "vgr-year");
    }
    if (IA.zoom) vgrD3.button(controls, "Återställ", function () { svg.transition().duration(dur).call(zoom.transform, d3.zoomIdentity); }, "Återställ zoom");
    controls.append("span").attr("class", "vgr-spacer");
    if (IA.export) vgrD3.exportButtons(controls, svg.node(), fileName);
    if (controls.selectAll("button,select,input").empty()) controls.remove();

    // Start
    if (spec.time.enabled) setPeriod(state.period);
    else { drawHeat(false); drawPoints(); }
    drawLegend();
  };
})();
