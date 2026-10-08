/* ── Journey world map: dotted world map with blinking pins ──
   Each .world-map element lists its stops in data-stops as
   [{ "n": 1, "lon": .., "lat": .., "dx": .., "dy": .., "usa": false }, ...].
   Land is drawn as a grid of dots (Brazil and the USA highlighted);
   a dashed route links the stops in order, arcing wide over the Atlantic
   on the hop to the US, like a flight path over the globe.
   Stops that sit too close together get their numbered pin nudged by
   (dx, dy) px, with a thin leader line back to the real location.
   Map data: Natural Earth via world-atlas (assets/data/countries-110m.json). */
(function () {
  const maps = document.querySelectorAll('.world-map');
  if (!maps.length || !window.d3 || !window.topojson) return;

  const LAND = 'rgba(115, 105, 100, 0.32)';
  const BRAZIL = 'rgba(139, 58, 42, 0.62)';
  const USA = 'rgba(11, 110, 87, 0.62)';
  const SVG_NS = 'http://www.w3.org/2000/svg';

  fetch('/assets/data/countries-110m.json')
    .then(function (r) { return r.json(); })
    .then(function (world) {
      const all = topojson.feature(world, world.objects.countries).features
        .filter(function (f) { return f.id !== '010'; });          // drop Antarctica
      const land = { type: 'FeatureCollection', features: all };
      const brazil = all.filter(function (f) { return f.id === '076'; });
      const usa = all.filter(function (f) { return f.id === '840'; });

      maps.forEach(function (el) {
        const stops = JSON.parse(el.dataset.stops || '[]');
        let lastWidth = 0;
        const render = function () {
          const W = el.clientWidth;
          if (!W || W === lastWidth) return;                       // hidden (other language) or unchanged
          lastWidth = W;
          draw(el, W, stops, land, brazil, usa);
        };
        render();
        if ('ResizeObserver' in window) new ResizeObserver(render).observe(el);
        else window.addEventListener('resize', render);
      });
    })
    .catch(function () { /* map data unavailable: leave the area empty */ });

  function draw(el, W, stops, land, brazil, usa) {
    // data-fit="076,..." zooms onto those countries and fills the element's
    // own box (regional map); otherwise the whole world is shown.
    const fit = (el.dataset.fit || '').split(',').filter(Boolean);
    let projection, H;
    if (fit.length) {
      H = el.clientHeight || Math.round(W * 1.25);
      const pad = W * 0.1;
      const target = { type: 'FeatureCollection',
                       features: land.features.filter(function (f) { return fit.indexOf(f.id) !== -1; }) };
      projection = d3.geoMercator().fitExtent([[pad, pad], [W - pad, H - pad]], target);
    } else {
      projection = d3.geoEqualEarth().fitWidth(W, land);
      H = Math.ceil(d3.geoPath(projection).bounds(land)[1][1] + 2);
    }
    const dpr = Math.min(window.devicePixelRatio || 1, 2);

    // Mask: land, Brazil and the USA painted in separate colour channels
    const mask = document.createElement('canvas');
    mask.width = W; mask.height = H;
    const m = mask.getContext('2d');
    const fill = function (features, color) {
      m.fillStyle = color;
      features.forEach(function (f) { m.beginPath(); d3.geoPath(projection, m)(f); m.fill(); });
    };
    fill(land.features, 'rgb(255,0,0)');
    fill(brazil, 'rgb(0,255,0)');
    fill(usa, 'rgb(0,0,255)');
    const px = m.getImageData(0, 0, W, H).data;

    // Dot grid
    const canvas = el.querySelector('.world-map-canvas');
    canvas.width = W * dpr; canvas.height = H * dpr;
    canvas.style.height = H + 'px';
    const ctx = canvas.getContext('2d');
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, W, H);
    const step = Math.max(4.5, W / 190);
    const r = step * 0.3;
    for (let y = step / 2; y < H; y += step) {
      for (let x = step / 2; x < W; x += step) {
        const i = ((y | 0) * W + (x | 0)) * 4;
        if (px[i + 1] > 128) ctx.fillStyle = BRAZIL;
        else if (px[i + 2] > 128) ctx.fillStyle = USA;
        else if (px[i] > 128) ctx.fillStyle = LAND;
        else continue;
        ctx.beginPath();
        ctx.arc(x, y, r, 0, Math.PI * 2);
        ctx.fill();
      }
    }

    // Route and leader lines
    const svg = el.querySelector('.world-map-lines');
    svg.setAttribute('viewBox', '0 0 ' + W + ' ' + H);
    while (svg.firstChild) svg.removeChild(svg.firstChild);
    el.querySelectorAll('.map-pin, .map-true-dot').forEach(function (n) { n.remove(); });

    const at = function (s) { return projection([s.lon, s.lat]); };
    const scale = Math.min(1, W / 900);                         // pull nudged pins in on small maps
    const pinAt = function (s) {
      const p = at(s);
      return [p[0] + (s.dx || 0) * scale, p[1] + (s.dy || 0) * scale];
    };

    // Route through the stops in order. Short hops bow slightly; the hop to
    // the US arcs wide to the right, over the Atlantic, like a flight path.
    if (stops.length > 1) {
      let d = '';
      for (let i = 1; i < stops.length; i++) {
        const p0 = pinAt(stops[i - 1]), p1 = pinAt(stops[i]);
        const k = stops[i].usa ? 0.75 : 0.2;
        const mx = (p0[0] + p1[0]) / 2, my = (p0[1] + p1[1]) / 2;
        const dx = p1[0] - p0[0], dy = p1[1] - p0[1];
        if (i === 1) d += 'M' + p0[0] + ',' + p0[1];
        d += ' Q' + (mx - dy * k) + ',' + (my + dx * k) + ' ' + p1[0] + ',' + p1[1];
      }
      const route = document.createElementNS(SVG_NS, 'path');
      route.setAttribute('d', d);
      route.setAttribute('class', 'route');
      svg.appendChild(route);
    }

    stops.forEach(function (s) {
      const p = at(s);
      const ox = (s.dx || 0) * scale, oy = (s.dy || 0) * scale;
      if (ox || oy) {
        const line = document.createElementNS(SVG_NS, 'line');
        line.setAttribute('x1', p[0]); line.setAttribute('y1', p[1]);
        line.setAttribute('x2', p[0] + ox); line.setAttribute('y2', p[1] + oy);
        line.setAttribute('class', 'leader');
        svg.appendChild(line);
        const dot = document.createElement('span');
        dot.className = 'map-true-dot' + (s.usa ? ' usa' : '');
        dot.style.left = p[0] + 'px'; dot.style.top = p[1] + 'px';
        el.appendChild(dot);
      }
      const pin = document.createElement('span');
      pin.className = 'map-pin';
      pin.style.left = (p[0] + ox) + 'px';
      pin.style.top = (p[1] + oy) + 'px';
      const cls = s.usa ? ' usa' : '';
      pin.innerHTML = '<span class="map-pin-ping' + cls + '"></span><span class="map-pin-dot' + cls + '">' + s.n + '</span>';
      el.appendChild(pin);
    });
  }
})();
