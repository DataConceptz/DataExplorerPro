// DataExplorerPro JavaScript

// Auto-resize textareas
$(document).on('input', 'textarea', function() {
  this.style.height = 'auto';
  this.style.height = Math.min(this.scrollHeight, 200) + 'px';
});

// Copy to clipboard
function copyToClipboard(elementId) {
  var text = document.getElementById(elementId).innerText;
  navigator.clipboard.writeText(text).then(function() {
    Shiny.setInputValue('clipboard_copied', Math.random());
  });
}

// Update Ollama status badge
Shiny.addCustomMessageHandler('updateOllamaStatus', function(data) {
  var badge = document.getElementById('ollama_status_badge');
  var indicator = badge.querySelector('.status-indicator');
  var text = badge.querySelector('span');
  
  indicator.classList.remove('warning', 'error');
  if (data.status === 'warning') {
    indicator.classList.add('warning');
  } else if (data.status === 'error') {
    indicator.classList.add('error');
  }
  
  text.textContent = data.message;
});

// Auto-scroll chat
Shiny.addCustomMessageHandler('scrollChat', function(data) {
  var chat = document.querySelector('.chat-container');
  if (chat) {
    chat.scrollTop = chat.scrollHeight;
  }
});

// Keyboard shortcut: Ctrl+Enter to send message, Shift+Enter for newline
document.addEventListener('keydown', function(e) {
  var ta = document.querySelector('#chat_input');
  if (!ta) return;

  if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
    // Prevent default newline and trigger send
    e.preventDefault();
    var sendBtn = document.getElementById('send_message');
    if (sendBtn) sendBtn.click();
  }
  // Allow Shift+Enter to insert newline (default behavior)
});

// Focus chat input when AI Chat tab is selected
document.addEventListener('click', function(e) {
  var el = e.target;
  // Find tab link text
  var tabLink = el.closest('.nav-tabs li a');
  if (tabLink && tabLink.textContent.trim().indexOf('AI Chat') === 0) {
    setTimeout(function(){
      var ta = document.querySelector('#chat_input');
      if (ta) ta.focus();
    }, 120);
  }
});

// Accessibility: add aria labels to key inputs (run once)
try {
  var f = document.querySelector('input[type=file]'); if (f) f.setAttribute('aria-label', 'Upload data file');
  var sd = document.getElementById('sample_data'); if (sd) sd.setAttribute('aria-label', 'Select sample dataset');
  var chatInput = document.getElementById('chat_input'); if (chatInput) chatInput.setAttribute('aria-label', 'AI chat input');
  var sendBtn = document.getElementById('send_message'); if (sendBtn) sendBtn.setAttribute('aria-label', 'Send chat message');
  var createChartBtn = document.getElementById('create_chart'); if (createChartBtn) createChartBtn.setAttribute('aria-label', 'Create chart');
} catch(e) {}

// Sidebar collapse toggle with persistence
(function(){
  var toggle = document.getElementById('toggle_sidebar');
  var main = document.querySelector('.main-container');
  function applyState(collapsed){
    if (collapsed) {
      main.classList.add('collapsed-sidebar');
      if (toggle && toggle.querySelector('i')) {
        toggle.querySelector('i').classList.remove('fa-compress-alt');
        toggle.querySelector('i').classList.add('fa-expand-alt');
      }
    } else {
      main.classList.remove('collapsed-sidebar');
      if (toggle && toggle.querySelector('i')) {
        toggle.querySelector('i').classList.remove('fa-expand-alt');
        toggle.querySelector('i').classList.add('fa-compress-alt');
      }
    }
  }
  // Restore state
  try { var s = localStorage.getItem('dep_sidebar_collapsed'); if (s === '1') applyState(true); }
  catch(e){}

  if (toggle) {
    toggle.addEventListener('click', function(){
      var collapsed = main.classList.contains('collapsed-sidebar');
      applyState(!collapsed);
      try { localStorage.setItem('dep_sidebar_collapsed', (!collapsed) ? '1' : '0'); } catch(e){}
    });
  }
})();

// Intro.js tour handler
(function(){
  var tourBtn = document.getElementById('show_tour');
  function runTour(){
    if (!window.introJs) return;
    var intro = introJs();
    intro.setOptions({
      steps: [
        { element: document.querySelector('.brand-text h1'), intro: 'Welcome to DataExplorerPro — your AI data companion!' },
        { element: document.querySelector('.sidebar .section-title'), intro: 'Load data here using upload or sample datasets.' },
        { element: document.querySelector('.nav-tabs > li:nth-child(1) a'), intro: 'Use the tabs to switch between major features.' },
        { element: document.querySelector('.chart-main'), intro: 'This is the main visualization area where charts and outputs appear.' },
        { element: document.getElementById('ollama_status_badge'), intro: 'Ollama status is shown here. Click to configure.' }
      ],
      showProgress: true,
      exitOnOverlayClick: false
    });
    intro.start();
    try { localStorage.setItem('dep_seen_tour', '1'); } catch(e){}
  }
  // Auto-run if never seen
  try { if (!localStorage.getItem('dep_seen_tour')) { setTimeout(runTour, 800); } } catch(e){}
  if (tourBtn) tourBtn.addEventListener('click', runTour);
})();

// Theme switching + persistence/sync helper
window.depSetTheme = function(theme, options) {
  var opts = options || {};
  var nextTheme = theme || 'light';

  try {
    document.documentElement.setAttribute('data-theme', nextTheme);
    localStorage.setItem('dataexplorerpro-theme', nextTheme);
  } catch(e) {}

  try {
    var select = document.getElementById('theme_select');
    if (select && select.value !== nextTheme) {
      select.value = nextTheme;
    }
  } catch(e) {}

  if (opts.pushToShiny && window.Shiny && typeof Shiny.setInputValue === 'function') {
    Shiny.setInputValue('app_theme', nextTheme, { priority: 'event' });
  }
};

Shiny.addCustomMessageHandler('setTheme', function(theme) {
  window.depSetTheme(theme, { pushToShiny: false });
});

// Load saved theme on page load
var savedTheme = localStorage.getItem('dataexplorerpro-theme') || 'light';
window.depSetTheme(savedTheme, { pushToShiny: false });

// Sync server-side settings theme once Shiny is connected
document.addEventListener('shiny:connected', function() {
  try {
    var activeTheme = document.documentElement.getAttribute('data-theme') || 'light';
    Shiny.setInputValue('app_theme', activeTheme, { priority: 'event' });
  } catch(e) {}
});

// Apply saved compact layout state if present
try {
  var compact = localStorage.getItem('dep_compact_layout');
  if (compact === '1') {
    document.documentElement.classList.add('compact-layout');
  }
} catch(e){}

// Custom message handler for compact layout
Shiny.addCustomMessageHandler('setCompact', function(val) {
  try {
    if (val) {
      document.documentElement.classList.add('compact-layout');
      localStorage.setItem('dep_compact_layout', '1');
    } else {
      document.documentElement.classList.remove('compact-layout');
      localStorage.setItem('dep_compact_layout', '0');
    }
  } catch(e){}
});

// Copy text helper
Shiny.addCustomMessageHandler('copyText', function(data) {
  try {
    var el = document.getElementById(data.id);
    if (!el) return;
    var text = el.value || el.innerText || el.textContent || '';
    navigator.clipboard.writeText(text).then(function() {
      Shiny.setInputValue('clipboard_copied', Math.random());
    });
  } catch(e){}
});

// Fullscreen modal toggle for charts
(function() {
  function isNativeFullscreen() {
    return !!(document.fullscreenElement || document.webkitFullscreenElement || document.msFullscreenElement);
  }

  function getActiveModal() {
    var modals = Array.prototype.slice.call(document.querySelectorAll('.modal.in, .modal.show'));
    for (var i = modals.length - 1; i >= 0; i--) {
      var m = modals[i];
      if (m && window.getComputedStyle(m).display !== 'none') return m;
    }
    return null;
  }

  function resizePlotlyWithin(container) {
    if (!container || !window.Plotly || !window.Plotly.Plots) return;
    var plots = container.querySelectorAll('.js-plotly-plot');
    plots.forEach(function(plotEl) {
      try { window.Plotly.Plots.resize(plotEl); } catch (e) {}
    });
  }

  function updateFullscreenButtons() {
    var activeLabel = (isNativeFullscreen() || document.querySelector('.modal.dep-fs-fallback')) ? 'Exit Fullscreen' : 'Fullscreen';
    var labels = document.querySelectorAll('.btn-fullscreen');
    labels.forEach(function(btn) {
      btn.textContent = activeLabel;
    });
  }

  function applyFallbackFullscreen(modal, enabled) {
    if (!modal) return;
    if (enabled) {
      modal.classList.add('dep-fs-fallback');
      document.body.classList.add('dep-fs-lock');
      setTimeout(function() { resizePlotlyWithin(modal); }, 50);
    } else {
      modal.classList.remove('dep-fs-fallback');
      if (!document.querySelector('.modal.dep-fs-fallback')) {
        document.body.classList.remove('dep-fs-lock');
      }
    }
  }

  window.toggleFullscreenModal = function() {
    var modal = getActiveModal() || document.querySelector('.modal.dep-fs-fallback');
    if (!modal) {
      console.log('No active modal found for fullscreen');
      return;
    }

    var target = modal.querySelector('.modal-content') || modal.querySelector('.modal-dialog') || modal;

    if (isNativeFullscreen() || modal.classList.contains('dep-fs-fallback')) {
      if (isNativeFullscreen()) {
        if (document.exitFullscreen) {
          document.exitFullscreen();
        } else if (document.webkitExitFullscreen) {
          document.webkitExitFullscreen();
        } else if (document.msExitFullscreen) {
          document.msExitFullscreen();
        }
      } else {
        applyFallbackFullscreen(modal, false);
      }
      updateFullscreenButtons();
      return;
    }

    if (target.requestFullscreen) {
      var fsResult;
      try {
        fsResult = target.requestFullscreen();
      } catch (err) {
        console.log('Fullscreen error; falling back to simulated fullscreen:', err);
        applyFallbackFullscreen(modal, true);
        updateFullscreenButtons();
        return;
      }

      // Some browsers return a Promise; some return void.
      if (fsResult && typeof fsResult.then === 'function') {
        fsResult.then(function() {
          setTimeout(function() { resizePlotlyWithin(modal); }, 50);
        }).catch(function(err) {
          console.log('Fullscreen error; falling back to simulated fullscreen:', err);
          applyFallbackFullscreen(modal, true);
          updateFullscreenButtons();
        });
      } else {
        setTimeout(function() {
          if (!isNativeFullscreen()) applyFallbackFullscreen(modal, true);
          updateFullscreenButtons();
          resizePlotlyWithin(modal);
        }, 50);
      }
    } else if (target.webkitRequestFullscreen) {
      target.webkitRequestFullscreen();
      setTimeout(function() {
        if (!isNativeFullscreen()) applyFallbackFullscreen(modal, true);
        updateFullscreenButtons();
      }, 50);
    } else if (target.msRequestFullscreen) {
      target.msRequestFullscreen();
      setTimeout(function() {
        if (!isNativeFullscreen()) applyFallbackFullscreen(modal, true);
        updateFullscreenButtons();
      }, 50);
    } else {
      applyFallbackFullscreen(modal, true);
      updateFullscreenButtons();
    }
  };

  function onFullscreenChange() {
    updateFullscreenButtons();
    var active = document.fullscreenElement || document.webkitFullscreenElement || document.msFullscreenElement || getActiveModal();
    setTimeout(function() { resizePlotlyWithin(active); }, 50);
  }

  document.addEventListener('fullscreenchange', onFullscreenChange);
  document.addEventListener('webkitfullscreenchange', onFullscreenChange);
  document.addEventListener('msfullscreenchange', onFullscreenChange);

  // Bootstrap modal events are emitted through jQuery in BS3/BS4
  if (window.jQuery) {
    window.jQuery(document).on('shown.bs.modal', '.modal', function() {
      updateFullscreenButtons();
      var self = this;
      setTimeout(function() { resizePlotlyWithin(self); }, 50);
    });
    window.jQuery(document).on('hidden.bs.modal', '.modal', function() {
      this.classList.remove('dep-fs-fallback');
      if (!document.querySelector('.modal.dep-fs-fallback')) {
        document.body.classList.remove('dep-fs-lock');
      }
      updateFullscreenButtons();
    });
  }
})();
