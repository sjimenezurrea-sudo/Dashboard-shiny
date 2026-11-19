/* www/helpers.js
   Navegación del índice, portada, menús y selector Claro/Oscuro.
   Sustituye el helpers.js anterior COMPLETO por este archivo.
*/
(function () {
  'use strict';

  var CLAVE_TEMA = 'dashboard-tema';

  // Cierra desplegables Bootstrap 5 y el menú móvil.
  function cerrarMenus() {
    document.querySelectorAll('.navbar .dropdown-toggle.show').forEach(function (boton) {
      if (window.bootstrap && window.bootstrap.Dropdown) {
        try {
          var instancia = window.bootstrap.Dropdown.getInstance(boton);
          if (instancia) instancia.hide();
        } catch (error) {
          console.warn('No fue posible cerrar un desplegable Bootstrap:', error);
        }
      }
      boton.classList.remove('show');
      boton.setAttribute('aria-expanded', 'false');
    });

    document.querySelectorAll('.navbar .dropdown-menu.show').forEach(function (menu) {
      menu.classList.remove('show');
    });
    document.querySelectorAll('.navbar .dropdown.show').forEach(function (item) {
      item.classList.remove('show');
    });

    var colapsado = document.querySelector('.navbar .navbar-collapse.show');
    if (colapsado && window.bootstrap && window.bootstrap.Collapse) {
      try {
        var instanciaCollapse = window.bootstrap.Collapse.getInstance(colapsado) ||
          new window.bootstrap.Collapse(colapsado, { toggle: false });
        instanciaCollapse.hide();
      } catch (error) {
        console.warn('No fue posible cerrar el menú móvil:', error);
      }
    }
  }

  function cerrarPortada() {
    var portada = document.getElementById('portada-dashboard');
    if (!portada || portada.classList.contains('portada-oculta')) return;
    portada.classList.add('portada-oculta');
    portada.setAttribute('aria-hidden', 'true');
    window.setTimeout(function () {
      if (portada.isConnected) portada.remove();
    }, 650);
  }

  // Actualiza el atributo que utilizan las reglas de styles.css.
  function aplicarTema(tema) {
    var oscuro = tema === 'dark';
    var valor = oscuro ? 'dark' : 'light';
    document.documentElement.setAttribute('data-bs-theme', valor);
    document.body.setAttribute('data-bs-theme', valor);

    var boton = document.getElementById('theme-switch');
    if (boton) {
      boton.classList.toggle('is-dark', oscuro);
      boton.setAttribute('aria-checked', oscuro ? 'true' : 'false');
      boton.setAttribute('aria-label', oscuro ? 'Cambiar a modo claro' : 'Cambiar a modo oscuro');
    }
    // Permite que otro script adapte sus gráficos si escucha el evento.
    window.dispatchEvent(new CustomEvent('dashboard-tema-cambiado', {
      detail: { tema: valor }
    }));
  }

  function prepararBotonTema() {
    var boton = document.getElementById('theme-switch');
    if (!boton || boton.dataset.temaPreparado === '1') return;
    boton.dataset.temaPreparado = '1';

    // En ui.R el botón está antes de navbarPage(). Lo movemos dentro
    // del contenedor de la barra para que apliquen los estilos existentes.
    var barra = document.querySelector('.navbar .container-fluid');
    if (barra && boton.parentElement !== barra) barra.appendChild(boton);

    var guardado = 'light';
    try {
      guardado = window.localStorage.getItem(CLAVE_TEMA) || 'light';
    } catch (error) {
      // El botón sigue funcionando aunque el almacenamiento esté bloqueado.
    }
    aplicarTema(guardado);
  }

  // Los mensajes que ya envía server.R siguen teniendo el mismo nombre.
  function registrarMensajesShiny() {
    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) {
      window.setTimeout(registrarMensajesShiny, 100);
      return;
    }
    window.Shiny.addCustomMessageHandler('cerrar_menu', function () {
      cerrarMenus();
      window.setTimeout(cerrarMenus, 80);
    });
    window.Shiny.addCustomMessageHandler('cerrar_portada', cerrarPortada);
  }

  // Capturamos ANTES de otros manejadores de clic del botón del tema:
  // evita dobles cambios si otro script antiguo también tenía un listener.
  document.addEventListener('click', function (evento) {
    var botonTema = evento.target.closest('#theme-switch');
    if (!botonTema) return;

    evento.preventDefault();
    evento.stopImmediatePropagation();
    var actual = document.documentElement.getAttribute('data-bs-theme') || 'light';
    var siguiente = actual === 'dark' ? 'light' : 'dark';
    aplicarTema(siguiente);
    try {
      window.localStorage.setItem(CLAVE_TEMA, siguiente);
    } catch (error) {
      // Sin almacenamiento persistente, el cambio visual funciona igual.
    }
  }, true);

  document.addEventListener('click', function (evento) {
    if (evento.target.closest('#entrar_dashboard')) {
      cerrarPortada();
    }

    // Los actionLink de Shiny usan href="#". No desactivamos su evento
    // de Shiny: solo impedimos el salto visual al comienzo de la página.
    var enlace = evento.target.closest('a.action-button');
    if (enlace) {
      evento.preventDefault();
      window.setTimeout(function () { enlace.blur(); }, 0);
    }

    var desdeIndice = evento.target.closest(
      '.indice-dashboard a.action-button, ' +
      '.objetivo-enlaces a.action-button, ' +
      '.tarjeta-kpi a.action-button'
    );
    if (desdeIndice) {
      // Mientras Shiny ejecuta updateNavbarPage(), impedimos que aparezca
      // el menú del grupo en lugar de mostrar solo la pestaña elegida.
      document.body.classList.add('navegacion-desde-indice');
      cerrarMenus();
      window.setTimeout(cerrarMenus, 0);
      window.setTimeout(cerrarMenus, 120);
      window.setTimeout(cerrarMenus, 350);
    }

    if (evento.target.closest('.navbar .dropdown-item, .navbar a[data-bs-toggle="tab"]')) {
      window.setTimeout(cerrarMenus, 100);
    }
  });

  // Cuando se pulsa un grupo del menú superior, recupera su funcionamiento
  // habitual (el índice no bloquea los menús para siempre).
  document.addEventListener('pointerdown', function (evento) {
    if (evento.target.closest('.navbar .dropdown-toggle')) {
      cerrarMenus();
      document.body.classList.remove('navegacion-desde-indice');
    }
  }, true);

  document.addEventListener('shown.bs.tab', function (evento) {
    if (evento.target && evento.target.closest('.navbar')) {
      window.setTimeout(cerrarMenus, 80);
      window.scrollTo({ top: 0, behavior: 'smooth' });
    }
  });

  document.addEventListener('keydown', function (evento) {
    if (evento.key === 'Escape') cerrarPortada();
  });

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', prepararBotonTema);
  } else {
    prepararBotonTema();
  }
  registrarMensajesShiny();
})();

