/* ── DNA helix background: grey/wine/teal dots forming a spinning double helix ──
   Used by the home hero (canvas inside the hero section) and as a fixed,
   full-screen background on other pages (canvas with data-mode="fixed").
 */
(function () {
  const canvas = document.querySelector('canvas.dna-canvas');
  if (!canvas || !canvas.getContext) return;
  const ctx = canvas.getContext('2d');
  const fixed = canvas.dataset.mode === 'fixed';
  const host = fixed ? null : canvas.parentElement;
  const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  let w, h, H, dpr, halfLen, helixR, cx, cy, points = [];
  let falling = [];            // dots knocked off the helix by the cursor
  let px = -1e4, py = -1e4, moved = 0;   // cursor position (px) and distance moved since last frame
  const BRUSH = 70;            // cursor radius that knocks dots off
  let spinBoost = 0;           // extra spin speed while the cursor moves
  let spin = 0, targetX = 0, targetY = 0, mouseX = 0, mouseY = 0;
  let running = false, visible = true, last = 0;
  const TURNS = 3.2;          // helix turns across the screen
  const TILT = -0.32;         // diagonal angle of the helix axis (radians)
  // Strand colours: the site's wine and teal; loose dots stay grey
  const COLORS = ['rgb(139, 58, 42)', 'rgb(11, 110, 87)', 'rgb(115, 105, 100)'];

  function hostRect() {
    return fixed ? { left: 0, top: 0, width: window.innerWidth, height: window.innerHeight }
                 : host.getBoundingClientRect();
  }

  function build() {
    dpr = Math.min(window.devicePixelRatio || 1, 2);
    w = fixed ? window.innerWidth : host.clientWidth;
    H = fixed ? window.innerHeight : host.clientHeight;
    h = Math.min(H, window.innerHeight);
    canvas.width = w * dpr;
    canvas.height = H * dpr;
    canvas.style.height = H + 'px';

    const small = w < 820;
    halfLen = Math.hypot(w, h) * 0.58;                 // long enough to run off both edges
    helixR = Math.min(h * (small ? 0.12 : 0.17), w * 0.2);
    cx = w / 2;
    cy = h / 2 + h * 0.04;

    points = [];
    const rnd = function (s) { return (Math.random() - 0.5) * s; };

    // Two backbone strands, each a fuzzy ribbon of dots
    const perStrand = Math.round(Math.min(1400, w * (small ? 0.9 : 0.75)));
    for (let s = 0; s < 2; s++) {
      for (let i = 0; i < perStrand; i++) {
        const u = Math.random() * 2 - 1;
        points.push({ u: u, a: s * Math.PI, r: 1 + rnd(0.16), off: rnd(0.03), size: 1, c: s });
      }
    }

    // Base-pair rungs connecting the strands
    const rungs = Math.round(TURNS * (small ? 8 : 11));
    for (let k = 0; k < rungs; k++) {
      const u = -1 + (k + 0.5) * (2 / rungs);
      const dots = small ? 9 : 13;
      for (let j = 0; j < dots; j++) {
        // r runs from +1 to -1: across the helix through its axis
        points.push({ u: u + rnd(0.004), a: 0, r: 1 - (2 * (j + 0.5)) / dots, off: rnd(0.02), size: 0.8, c: j < dots / 2 ? 0 : 1 });
      }
    }

    // Loose ambient dots floating around the helix
    const loose = small ? 90 : 220;
    for (let i = 0; i < loose; i++) {
      points.push({ u: Math.random() * 2 - 1, a: Math.random() * Math.PI * 2, r: 1.4 + Math.random() * 1.6, off: 0, size: 0.7, c: 2 });
    }

    points.forEach(function (p) {
      p.phase = Math.random() * Math.PI * 2;
      p.speed = 0.4 + Math.random() * 0.8;
      p.amp = 0.03 + Math.random() * 0.05;
    });
  }

  function draw(t, dt) {
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, w, H);

    mouseX += (targetX - mouseX) * 0.04;
    mouseY += (targetY - mouseY) * 0.04;
    const tilt = TILT + mouseY * 0.08;
    const cosT = Math.cos(tilt), sinT = Math.sin(tilt);
    const time = t / 1000;
    const twist = TURNS * Math.PI * 2;
    const fov = 3.2;

    const nearby = [];           // helix dots under the cursor this frame

    for (const p of points) {
      const wob = Math.sin(time * p.speed + p.phase) * p.amp;
      const ang = p.u * twist + p.a + spin + mouseX * 0.6;
      const r = (p.r + wob) * helixR;

      // Helix in its own frame: x along the axis, y/z around it
      const ax = (p.u + p.off) * halfLen;
      const ay = Math.cos(ang) * r + Math.cos(time * p.speed * 0.7 + p.phase) * p.amp * helixR;
      const az = Math.sin(ang) * r;

      const zn = az / (helixR * 3);                  // roughly -1 (back) .. 1 (front)
      const scale = fov / (fov - zn);
      const sx = cx + (ax * cosT - ay * sinT);
      const sy = cy + (ax * sinT + ay * cosT) * scale;
      const front = Math.max(0, Math.min(1, (zn + 1) / 2));

      if (moved && p.size > 0.7 && Math.abs(sx - px) < BRUSH && Math.abs(sy - py) < BRUSH &&
          (sx - px) * (sx - px) + (sy - py) * (sy - py) < BRUSH * BRUSH) {
        nearby.push({ x: sx, y: sy, c: p.c, a: 0.22 + front * 0.38, size: (0.8 + front * 1.2) * scale });
      }

      ctx.fillStyle = COLORS[p.c];
      ctx.globalAlpha = 0.1 + front * 0.4;
      ctx.beginPath();
      ctx.arc(sx, sy, (0.6 + front * 1.2) * p.size * scale, 0, Math.PI * 2);
      ctx.fill();
    }

    // The faster the cursor moves through the helix, the more dots break off
    const spawn = Math.min(Math.round(moved * 0.25), 20, nearby.length, 500 - falling.length);
    for (let k = 0; k < spawn; k++) {
      const n = nearby[(Math.random() * nearby.length) | 0];
      const dir = Math.random() < 0.5 ? -1 : 1;  // straight up or straight down
      falling.push({ x: n.x, y: n.y, c: n.c, y0: n.y, dir: dir, v: 0.02 + Math.random() * 0.05, a: n.a, size: n.size });
    }
    moved = 0;

    // Released dots travel vertically only, fading out before the edge
    if (dt) {
      for (let i = falling.length - 1; i >= 0; i--) {
        const f = falling[i];
        f.v = Math.min(f.v + 0.00005 * dt, 0.3);
        f.y += f.dir * f.v * dt;
        const edge = f.dir > 0 ? H : 0;
        const progress = Math.abs(f.y - f.y0) / Math.max(Math.abs(edge - f.y0), 1);
        if (progress >= 1) { falling.splice(i, 1); continue; }
        ctx.fillStyle = COLORS[f.c];
        ctx.globalAlpha = f.a * (1 - progress * progress);
        ctx.beginPath();
        ctx.arc(f.x, f.y, f.size * (1 - progress * 0.4), 0, Math.PI * 2);
        ctx.fill();
      }
    }
    ctx.globalAlpha = 1;
  }

  function frame(t) {
    if (!running) return;
    const dt = last ? Math.min(t - last, 50) : 16;
    last = t;
    spin += dt * (0.00025 + spinBoost);
    spinBoost *= Math.pow(0.97, dt / 16);   // ease back to normal speed
    draw(t, dt);
    requestAnimationFrame(frame);
  }

  function start() {
    if (running || reduceMotion || !visible || document.hidden) return;
    running = true; last = 0;
    requestAnimationFrame(frame);
  }
  function stop() { running = false; }

  build();
  draw(0);
  start();

  let resizeTimer;
  window.addEventListener('resize', function () {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(function () { build(); falling = []; draw(performance.now()); }, 150);
  });

  const pointerTarget = fixed ? window : host;
  pointerTarget.addEventListener('pointermove', function (e) {
    const r = hostRect();
    targetX = (e.clientX - r.left) / r.width - 0.5;
    targetY = (e.clientY - r.top) / r.height - 0.5;
    const nx = e.clientX - r.left, ny = e.clientY - r.top;
    if (px > -1e3) {
      const d = Math.hypot(nx - px, ny - py);
      moved += d;
      spinBoost = Math.min(spinBoost + d * 0.000004, 0.002);
    }
    px = nx; py = ny;
  });
  (fixed ? document.documentElement : host).addEventListener('pointerleave', function () { px = py = -1e4; moved = 0; });

  document.addEventListener('visibilitychange', function () { document.hidden ? stop() : start(); });
  if (!fixed && 'IntersectionObserver' in window) {
    new IntersectionObserver(function (entries) {
      visible = entries[0].isIntersecting;
      visible ? start() : stop();
    }).observe(host);
  }
})();
