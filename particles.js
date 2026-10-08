/* ── Particle background: drifting dots and small DNA / RNA fragments ──
   A canvas.particles-canvas either fills its parent section (home awards)
   or, with data-mode="fixed", sits behind the whole page. DNA pieces under
   the cursor unzip into two RNA strands. */
(function () {
  const canvas = document.querySelector('canvas.particles-canvas');
  if (!canvas || !canvas.getContext) return;
  const ctx = canvas.getContext('2d');
  const fixed = canvas.dataset.mode === 'fixed';
  const wrap = canvas.parentElement;
  const reduceMotion = window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  const WINE = 'rgb(139, 58, 42)', TEAL = 'rgb(11, 110, 87)', GREY = 'rgb(115, 105, 100)';

  let w = 0, h = 0, dpr = 1, dots = [], frags = [];
  let running = false, visible = false, last = 0;
  let px = -1e4, py = -1e4;    // cursor position over the canvas
  let respawn = [];            // countdowns (ms) until a new DNA piece appears

  function makeFrag(rna, alpha) {
    return {
      rna: rna, alpha: alpha, ttl: Infinity, svx: 0, svy: 0,
      x: Math.random() * w, y: Math.random() * h,
      vx: (Math.random() - 0.5) * 0.01, vy: (Math.random() - 0.5) * 0.008,
      len: 90 + Math.random() * 110,
      rad: 9 + Math.random() * 7,
      angle: Math.random() * Math.PI * 2,
      turn: (Math.random() - 0.5) * 0.00008,             // slow rotation of the whole piece
      spin: Math.random() * Math.PI * 2,
      spinSpeed: 0.0006 + Math.random() * 0.0006,        // twist around its own axis
      color: Math.random() < 0.5 ? WINE : TEAL
    };
  }

  // Unzip a DNA piece into its two strands, which drift apart as RNA
  // and fade away after a while; a fresh DNA piece appears elsewhere
  function split(f) {
    const nx = -Math.sin(f.angle), ny = Math.cos(f.angle);
    const push = 0.035;
    frags.splice(frags.indexOf(f), 1);
    [[WINE, 0, 1], [TEAL, Math.PI, -1]].forEach(function (s) {
      frags.push(Object.assign({}, f, {
        rna: true, color: s[0], spin: f.spin + s[1],
        svx: nx * push * s[2], svy: ny * push * s[2],
        ttl: 9000 + Math.random() * 5000
      }));
    });
    respawn.push(2500 + Math.random() * 2500);
  }

  function build() {
    dpr = Math.min(window.devicePixelRatio || 1, 2);
    w = fixed ? window.innerWidth : wrap.clientWidth;
    h = fixed ? window.innerHeight : wrap.clientHeight;
    canvas.width = w * dpr;
    canvas.height = h * dpr;

    dots = [];
    const nDots = Math.round(Math.min(340, (w * h) / 7000));
    for (let i = 0; i < nDots; i++) {
      dots.push({
        x: Math.random() * w, y: Math.random() * h,
        vx: (Math.random() - 0.5) * 0.012, vy: (Math.random() - 0.5) * 0.012,
        r: 0.7 + Math.random() * 1.3, a: 0.12 + Math.random() * 0.25,
        c: Math.random() < 0.4 ? GREY : (Math.random() < 0.5 ? WINE : TEAL)
      });
    }

    frags = [];
    respawn = [];
    const nFrags = Math.max(4, Math.min(12, Math.round((w * h) / 140000)));
    for (let i = 0; i < nFrags; i++) {
      frags.push(makeFrag(i % 3 === 2, 1));                // roughly a third are RNA
    }
  }

  function dot(x, y, r, a, c) {
    ctx.globalAlpha = a;
    ctx.fillStyle = c;
    ctx.beginPath();
    ctx.arc(x, y, r, 0, Math.PI * 2);
    ctx.fill();
  }

  function drawFragment(f) {
    const cosA = Math.cos(f.angle), sinA = Math.sin(f.angle);
    const steps = Math.round(f.len / 5);
    const twist = (f.len / 70) * Math.PI;
    // Fade the ends so the fragment looks like a broken-off piece
    for (let i = 0; i <= steps; i++) {
      const u = i / steps - 0.5;
      const edge = (1 - Math.pow(Math.abs(u) * 2, 3)) * f.alpha;
      const along = u * f.len;
      const ang = u * twist + f.spin;
      const strands = f.rna ? [0] : [0, Math.PI];

      strands.forEach(function (off, s) {
        const off2 = Math.cos(ang + off) * f.rad;
        const depth = (Math.sin(ang + off) + 1) / 2;   // 0 back .. 1 front
        const x = f.x + along * cosA - off2 * sinA;
        const y = f.y + along * sinA + off2 * cosA;
        const c = f.rna ? f.color : (s === 0 ? WINE : TEAL);
        dot(x, y, 0.7 + depth * 1.1, (0.1 + depth * 0.3) * edge, c);
      });

      // DNA: base-pair rungs between the strands; RNA: short bases on one side
      if (i % 3 === 1) {
        const a1 = Math.cos(ang) * f.rad;
        const span = f.rna ? [0.35, 0.7] : [-0.6, -0.2, 0.2, 0.6];
        span.forEach(function (k) {
          const off2 = f.rna ? a1 + k * f.rad * 1.4 : a1 * k * -1;
          const x = f.x + along * cosA - off2 * sinA;
          const y = f.y + along * sinA + off2 * cosA;
          dot(x, y, 0.7, 0.16 * edge, f.rna ? GREY : (k < 0 ? WINE : TEAL));
        });
      }
    }
  }

  function draw(dt) {
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, w, h);
    const pad = 120;

    dots.forEach(function (d) {
      d.x += d.vx * dt; d.y += d.vy * dt;
      if (d.x < -5) d.x = w + 5; else if (d.x > w + 5) d.x = -5;
      if (d.y < -5) d.y = h + 5; else if (d.y > h + 5) d.y = -5;
      dot(d.x, d.y, d.r, d.a, d.c);
    });

    // DNA pieces under the cursor unzip into two RNA strands
    if (dt && px > -1e3) {
      frags.filter(function (f) { return !f.rna && f.alpha > 0.6; }).forEach(function (f) {
        const dx = px - f.x, dy = py - f.y;
        const along = dx * Math.cos(f.angle) + dy * Math.sin(f.angle);
        const perp = -dx * Math.sin(f.angle) + dy * Math.cos(f.angle);
        if (Math.abs(along) < f.len * 0.45 && Math.abs(perp) < f.rad + 18) split(f);
      });
    }

    for (let i = respawn.length - 1; i >= 0; i--) {
      respawn[i] -= dt;
      if (respawn[i] <= 0) { respawn.splice(i, 1); frags.push(makeFrag(false, 0)); }
    }

    const decay = Math.pow(0.985, dt / 16);
    for (let i = frags.length - 1; i >= 0; i--) {
      const f = frags[i];
      f.x += (f.vx + f.svx) * dt; f.y += (f.vy + f.svy) * dt;
      f.svx *= decay; f.svy *= decay;
      f.angle += f.turn * dt;
      f.spin += f.spinSpeed * dt;
      if (f.ttl !== Infinity) {
        f.ttl -= dt;
        if (f.ttl <= 0) { frags.splice(i, 1); continue; }
        f.alpha = Math.min(1, f.ttl / 2000);              // fade out over the last 2 s
      } else if (f.alpha < 1) {
        f.alpha = Math.min(1, f.alpha + dt / 1500);       // new pieces fade in
      }
      if (f.x < -pad) f.x = w + pad; else if (f.x > w + pad) f.x = -pad;
      if (f.y < -pad) f.y = h + pad; else if (f.y > h + pad) f.y = -pad;
      drawFragment(f);
    }
    ctx.globalAlpha = 1;
  }

  function frame(t) {
    if (!running) return;
    const dt = last ? Math.min(t - last, 50) : 16;
    last = t;
    draw(dt);
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

  // Rebuild when the area changes size (window resize, language switch)
  if (fixed) {
    let resizeTimer;
    window.addEventListener('resize', function () {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(function () { build(); draw(0); }, 150);
    });
  } else if ('ResizeObserver' in window) {
    new ResizeObserver(function () {
      if (wrap.clientWidth !== w || wrap.clientHeight !== h) { build(); draw(0); }
    }).observe(wrap);
  }
  const area = fixed ? document.documentElement : wrap;
  area.addEventListener('pointermove', function (e) {
    const r = fixed ? { left: 0, top: 0 } : wrap.getBoundingClientRect();
    px = e.clientX - r.left; py = e.clientY - r.top;
  });
  area.addEventListener('pointerleave', function () { px = py = -1e4; });
  document.addEventListener('visibilitychange', function () { document.hidden ? stop() : start(); });
  if (!fixed && 'IntersectionObserver' in window) {
    new IntersectionObserver(function (entries) {
      visible = entries[0].isIntersecting;
      visible ? start() : stop();
    }).observe(wrap);
  } else {
    visible = true; start();
  }
})();
