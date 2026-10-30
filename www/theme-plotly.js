(function () {
  function applyTheme() {
    var dark = document.documentElement.getAttribute('data-bs-theme') === 'dark' ||
      document.body.getAttribute('data-bs-theme') === 'dark' ||
      document.documentElement.getAttribute('data-bs-color-mode') === 'dark';
    var text = dark ? '#EFF3F9' : '#202936';
    var grid = dark ? '#465266' : '#E7E1DC';
    if (!window.Plotly) return;
    document.querySelectorAll('.js-plotly-plot').forEach(function(el) {
      if (!el._fullLayout) return;
      var upd = {'font.color': text};
      ['xaxis','yaxis','xaxis2','yaxis2'].forEach(function(ax) {
        if (el.layout && el.layout[ax]) {
          upd[ax + '.tickfont.color'] = text;
          upd[ax + '.title.font.color'] = text;
          upd[ax + '.gridcolor'] = grid;
        }
      });
      try { window.Plotly.relayout(el, upd); } catch(e) { /* gráficos aún en proceso de render */ }
    });
  }
  document.addEventListener('DOMContentLoaded',function() {
    var obs = new MutationObserver(function() { window.requestAnimationFrame(applyTheme); });
    obs.observe(document.documentElement,{attributes:true,attributeFilter:['data-bs-theme','data-bs-color-mode']});
    obs.observe(document.body,{attributes:true,attributeFilter:['data-bs-theme','data-bs-color-mode']});
    document.addEventListener('shown.bs.tab',applyTheme);
    document.addEventListener('shiny:value',function(e) {
      if (e.target && e.target.classList && e.target.classList.contains('shiny-plot-output')) setTimeout(applyTheme,70);
    });
    applyTheme();
  });
})();
