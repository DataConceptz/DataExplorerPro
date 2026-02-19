# DataExplorerPro UI Definition v2.0
# AI-Powered Data Exploration Studio - Modern Glassmorphism Design

library(shiny)
library(plotly)
library(DT)

# ═══════════════════════════════════════════════════════════════════════════════
# BEAUTIFUL MODERN UI
# ═══════════════════════════════════════════════════════════════════════════════

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@300;400;500;600;700&family=Fraunces:opsz,wght@9..144,400;500;600;700&family=JetBrains+Mono:wght@400;500;600&display=swap", rel = "stylesheet"),
    tags$link(href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css", rel = "stylesheet"),
    # Dark Theme Stylesheet
    tags$link(rel = "stylesheet", href = "custom.css"),
    # Intro.js for guided tours
    tags$link(rel = "stylesheet", href = "https://unpkg.com/intro.js/minified/introjs.min.css"),
    tags$script(src = "https://unpkg.com/intro.js/minified/intro.min.js"),
    tags$style(HTML('
      /* ═══════════════════════════════════════════════════════════════════════════
         CSS VARIABLES — GLASSMORPHISM DESIGN SYSTEM
         ═══════════════════════════════════════════════════════════════════════════ */
      :root {
        /* Glassmorphism backgrounds */
        --bg-primary: #f0f4f8;
        --bg-secondary: #e2e8f0;
        --bg-tertiary: #cbd5e1;
        --bg-card: rgba(255, 255, 255, 0.65);
        --bg-glass: rgba(255, 255, 255, 0.45);
        --bg-glass-hover: rgba(255, 255, 255, 0.65);

        /* Frosted surfaces */
        --surface-header: rgba(255, 255, 255, 0.72);
        --surface-nav: rgba(255, 255, 255, 0.58);
        --surface-control: rgba(255, 255, 255, 0.55);
        --surface-card-header: linear-gradient(180deg, rgba(99, 102, 241, 0.06) 0%, transparent 100%);
        --surface-card-footer: rgba(248, 250, 252, 0.6);
        --surface-input: rgba(255, 255, 255, 0.75);
        --surface-modal: rgba(255, 255, 255, 0.85);
        --surface-badge: rgba(255, 255, 255, 0.6);
        --surface-table-head: rgba(248, 250, 252, 0.7);
        
        /* Premium accent palette */
        --accent-primary: #6366f1;
        --accent-secondary: #8b5cf6;
        --accent-tertiary: #a855f7;
        --accent-success: #10b981;
        --accent-warning: #f59e0b;
        --accent-danger: #ef4444;
        --accent-info: #3b82f6;
        
        /* Text hierarchy */
        --text-primary: #1e293b;
        --text-secondary: #475569;
        --text-muted: #64748b;
        --text-dim: #94a3b8;
        
        /* Glass borders */
        --border-subtle: rgba(148, 163, 184, 0.2);
        --border-accent: rgba(99, 102, 241, 0.35);
        --border-glow: rgba(99, 102, 241, 0.5);
        
        /* Premium gradients */
        --gradient-primary: linear-gradient(135deg, #6366f1 0%, #8b5cf6 50%, #a855f7 100%);
        --gradient-secondary: linear-gradient(135deg, #3b82f6 0%, #6366f1 100%);
        --gradient-warm: linear-gradient(135deg, #f59e0b 0%, #ef4444 100%);
        --gradient-surface: linear-gradient(180deg, rgba(99, 102, 241, 0.06) 0%, transparent 100%);
        --gradient-glass: linear-gradient(135deg, rgba(255,255,255,0.4) 0%, rgba(255,255,255,0.1) 100%);
        
        /* Glassmorphism shadows */
        --shadow-sm: 0 2px 8px rgba(15, 23, 42, 0.06);
        --shadow-md: 0 8px 24px rgba(15, 23, 42, 0.1);
        --shadow-lg: 0 16px 48px rgba(15, 23, 42, 0.15);
        --shadow-glow: 0 0 40px rgba(99, 102, 241, 0.15);
        --shadow-glass: 0 8px 32px rgba(15, 23, 42, 0.12), inset 0 1px 0 rgba(255,255,255,0.5);
        
        /* Blur values */
        --blur-sm: 8px;
        --blur-md: 16px;
        --blur-lg: 24px;
        --blur-xl: 40px;
        
        --radius-xs: 6px;
        --radius-sm: 10px;
        --radius-md: 14px;
        --radius-lg: 20px;
        --radius-xl: 28px;
        --radius-full: 9999px;
        
        --transition-fast: 0.15s cubic-bezier(0.4, 0, 0.2, 1);
        --transition-normal: 0.25s cubic-bezier(0.4, 0, 0.2, 1);
        --transition-slow: 0.4s cubic-bezier(0.4, 0, 0.2, 1);
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         THEME DEFINITIONS
         ═══════════════════════════════════════════════════════════════════════════ */
      
      /* Light Theme — Glassmorphism */
      [data-theme="light"] {
        --bg-primary: #f0f4f8;
        --bg-secondary: #e2e8f0;
        --bg-tertiary: #cbd5e1;
        --bg-card: rgba(255, 255, 255, 0.65);
        --bg-glass: rgba(255, 255, 255, 0.45);
        --bg-glass-hover: rgba(255, 255, 255, 0.65);

        --surface-header: rgba(255, 255, 255, 0.72);
        --surface-nav: rgba(255, 255, 255, 0.58);
        --surface-control: rgba(248, 250, 252, 0.55);
        --surface-card-header: linear-gradient(180deg, rgba(99, 102, 241, 0.06) 0%, transparent 100%);
        --surface-card-footer: rgba(248, 250, 252, 0.6);
        --surface-input: rgba(255, 255, 255, 0.75);
        --surface-modal: rgba(255, 255, 255, 0.85);
        --surface-badge: rgba(248, 250, 252, 0.6);
        --surface-table-head: rgba(248, 250, 252, 0.7);
        
        --text-primary: #1e293b;
        --text-secondary: #475569;
        --text-muted: #64748b;
        --text-dim: #94a3b8;
        
        --border-subtle: rgba(148, 163, 184, 0.2);
        --border-accent: rgba(99, 102, 241, 0.35);
        --border-glow: rgba(99, 102, 241, 0.5);
        
        --shadow-sm: 0 2px 8px rgba(15, 23, 42, 0.06);
        --shadow-md: 0 8px 24px rgba(15, 23, 42, 0.1);
        --shadow-lg: 0 16px 48px rgba(15, 23, 42, 0.15);
        --shadow-glow: 0 0 40px rgba(99, 102, 241, 0.15);
        --shadow-glass: 0 8px 32px rgba(15, 23, 42, 0.12), inset 0 1px 0 rgba(255,255,255,0.5);
      }
      
      /* Blue Theme */
      [data-theme="blue"] {
        --bg-primary: #0f172a;
        --bg-secondary: #1e293b;
        --bg-tertiary: #334155;
        --accent-primary: #3b82f6;
        --accent-secondary: #60a5fa;
        --accent-tertiary: #93c5fd;
      }
      
      /* Green Theme */
      [data-theme="green"] {
        --accent-primary: #10b981;
        --accent-secondary: #34d399;
        --accent-tertiary: #6ee7b7;
        --gradient-primary: linear-gradient(135deg, #10b981 0%, #34d399 50%, #6ee7b7 100%);
      }
      
      /* Purple Theme */
      [data-theme="purple"] {
        --accent-primary: #0ea5a4;
        --accent-secondary: #5eead4;
        --accent-tertiary: #c4b5fd;
        --gradient-primary: linear-gradient(135deg, #0ea5a4 0%, #5eead4 50%, #c4b5fd 100%);
      }
      
      /* Orange Theme */
      [data-theme="orange"] {
        --accent-primary: #f97316;
        --accent-secondary: #fb923c;
        --accent-tertiary: #fdba74;
        --gradient-primary: linear-gradient(135deg, #f97316 0%, #fb923c 50%, #fdba74 100%);
      }
      
      /* Red Theme */
      [data-theme="red"] {
        --accent-primary: #ef4444;
        --accent-secondary: #f87171;
        --accent-tertiary: #fca5a5;
        --gradient-primary: linear-gradient(135deg, #ef4444 0%, #f87171 50%, #fca5a5 100%);
      }
      
      /* Monochrome Theme */
      [data-theme="monochrome"] {
        --accent-primary: #6b7280;
        --accent-secondary: #9ca3af;
        --accent-tertiary: #d1d5db;
        --gradient-primary: linear-gradient(135deg, #6b7280 0%, #9ca3af 50%, #d1d5db 100%);
      }

      /* Solarize Theme */
      [data-theme="solarize"] {
        --bg-primary: #002b36;
        --bg-secondary: #073642;
        --bg-tertiary: #0b3a46;
        --bg-card: rgba(7, 54, 66, 0.85);
        --bg-glass: rgba(253, 246, 227, 0.06);
        --bg-glass-hover: rgba(253, 246, 227, 0.1);

        --accent-primary: #268bd2;
        --accent-secondary: #2aa198;
        --accent-tertiary: #b58900;
        --accent-success: #859900;
        --accent-warning: #b58900;
        --accent-danger: #dc322f;
        --accent-info: #268bd2;

        --text-primary: #fdf6e3;
        --text-secondary: #eee8d5;
        --text-muted: #93a1a1;
        --text-dim: #839496;

        --border-subtle: rgba(238, 232, 213, 0.08);
        --border-accent: rgba(38, 139, 210, 0.3);
        --border-glow: rgba(38, 139, 210, 0.45);
      }

      /* Cobalt Theme */
      [data-theme="cobalt"] {
        --bg-primary: #0d1b2a;
        --bg-secondary: #1b263b;
        --bg-tertiary: #22304a;
        --bg-card: rgba(27, 38, 59, 0.85);
        --bg-glass: rgba(224, 231, 255, 0.04);
        --bg-glass-hover: rgba(224, 231, 255, 0.08);

        --accent-primary: #3b82f6;
        --accent-secondary: #60a5fa;
        --accent-tertiary: #93c5fd;
        --accent-success: #22c55e;
        --accent-warning: #f59e0b;
        --accent-danger: #ef4444;
        --accent-info: #38bdf8;

        --text-primary: #e2e8f0;
        --text-secondary: #cbd5f5;
        --text-muted: #94a3b8;
        --text-dim: #7c8aa5;

        --border-subtle: rgba(148, 163, 184, 0.12);
        --border-accent: rgba(59, 130, 246, 0.35);
        --border-glow: rgba(59, 130, 246, 0.55);
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         GLOBAL RESETS — GLASSMORPHISM BASE
         ═══════════════════════════════════════════════════════════════════════════ */
      *, *::before, *::after { 
        box-sizing: border-box; 
        margin: 0; 
        padding: 0; 
      }
      
      html { scroll-behavior: smooth; }
      
      body { 
        font-family: "Space Grotesk", "Segoe UI", sans-serif;
        font-size: 15px;
        line-height: 1.6;
        color: var(--text-primary);
        /* Glassmorphism mesh gradient background */
        background: 
          radial-gradient(ellipse 80% 50% at 20% 40%, rgba(99, 102, 241, 0.08), transparent 50%),
          radial-gradient(ellipse 60% 40% at 80% 20%, rgba(139, 92, 246, 0.06), transparent 50%),
          radial-gradient(ellipse 50% 30% at 60% 80%, rgba(168, 85, 247, 0.05), transparent 50%),
          radial-gradient(ellipse 70% 45% at 10% 90%, rgba(59, 130, 246, 0.05), transparent 50%),
          var(--bg-primary);
        min-height: 100vh;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
        letter-spacing: 0.1px;
      }

      h1, h2, h3, h4, h5, .section-title {
        font-family: "Fraunces", "Space Grotesk", serif;
        letter-spacing: -0.2px;
      }

      code, pre, .code-block {
        font-family: "JetBrains Mono", ui-monospace, SFMono-Regular, Menlo, Consolas, "Liberation Mono", monospace;
      }
      
      /* Ambient background glow - removed for performance */
      
      .container-fluid {
        position: relative;
        z-index: 1;
        padding: 0 !important;
        max-width: 100%;
      }

      .app-shell {
        max-width: 1600px;
        margin: 0 auto;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         SCROLLBAR — GLASS THEME
         ═══════════════════════════════════════════════════════════════════════════ */
      ::-webkit-scrollbar { width: 8px; height: 8px; }
      ::-webkit-scrollbar-track { background: transparent; border-radius: 10px; }
      ::-webkit-scrollbar-thumb { 
        background: rgba(99, 102, 241, 0.2);
        border-radius: 10px;
        transition: background 0.2s;
      }
      ::-webkit-scrollbar-thumb:hover { 
        background: rgba(99, 102, 241, 0.4);
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         HEADER — FROSTED GLASS
         ═══════════════════════════════════════════════════════════════════════════ */
      .app-header {
        position: sticky;
        top: 0;
        z-index: 100;
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 14px 28px;
        /* Frosted glass effect */
        background: var(--surface-header);
        backdrop-filter: blur(var(--blur-lg));
        -webkit-backdrop-filter: blur(var(--blur-lg));
        border-bottom: 1px solid var(--border-subtle);
        box-shadow: var(--shadow-glass);
        transition: background 0.3s ease, box-shadow 0.3s ease;
      }
      .app-header::after {
        content: "";
        position: absolute;
        bottom: 0;
        left: 0;
        right: 0;
        height: 1px;
        background: linear-gradient(90deg, transparent 0%, var(--border-accent) 50%, transparent 100%);
        opacity: 0.6;
      }
      
      .header-brand {
        display: flex;
        align-items: center;
        gap: 16px;
      }
      
      .brand-icon {
        width: 44px;
        height: 44px;
        background: var(--gradient-primary);
        border-radius: var(--radius-md);
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 20px;
        color: white;
        box-shadow: 0 4px 16px rgba(99, 102, 241, 0.35), inset 0 1px 0 rgba(255,255,255,0.2);
        position: relative;
        overflow: hidden;
        transition: transform var(--transition-normal), box-shadow var(--transition-normal);
      }

      .brand-icon:hover {
        transform: scale(1.08) rotate(-3deg);
        box-shadow: 0 6px 24px rgba(99, 102, 241, 0.5), inset 0 1px 0 rgba(255,255,255,0.2);
      }
      
      .brand-icon::before {
        content: "";
        position: absolute;
        inset: 0;
        background: linear-gradient(135deg, rgba(255,255,255,0.2) 0%, transparent 50%);
      }
      
      .brand-icon i { position: relative; z-index: 1; }
      
      .brand-text h1 {
        font-size: 24px;
        font-weight: 700;
        background: var(--gradient-primary);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        letter-spacing: -0.5px;
        margin: 0;
        line-height: 1.2;
      }
      
      .brand-text .tagline {
        font-size: 12px;
        color: var(--text-muted);
        font-weight: 500;
        letter-spacing: 0.3px;
        margin-top: 2px;
      }
      
      .header-controls {
        display: flex;
        align-items: center;
        gap: 16px;
      }
      
      .status-badge {
        display: flex;
        align-items: center;
        gap: 10px;
        padding: 10px 18px;
        background: var(--bg-glass);
        border: 1px solid var(--border-subtle);
        border-radius: var(--radius-full);
        font-size: 13px;
        font-weight: 500;
        color: var(--text-secondary);
        transition: background 0.15s ease, border-color 0.15s ease;
        cursor: pointer;
      }
      
      .status-badge:hover {
        background: var(--bg-glass-hover);
        border-color: var(--border-accent);
      }
      
      .status-indicator {
        width: 10px;
        height: 10px;
        border-radius: 50%;
        background: var(--accent-success);
        box-shadow: 0 0 8px var(--accent-success);
      }
      
      .status-indicator.warning { 
        background: var(--accent-warning); 
        box-shadow: 0 0 12px var(--accent-warning); 
      }
      
      .status-indicator.error { 
        background: var(--accent-danger); 
        box-shadow: 0 0 12px var(--accent-danger); 
        animation: none;
      }
      
      @keyframes pulse {
        0%, 100% { opacity: 1; transform: scale(1); }
        50% { opacity: 0.6; transform: scale(0.95); }
      }

      /* Theme Switcher */
      .theme-switcher { position: relative; }
      .theme-select {
        appearance: none;
        background: var(--bg-glass) !important;
        border: 1px solid var(--border-subtle) !important;
        border-radius: var(--radius-full) !important;
        padding: 8px 32px 8px 14px !important;
        font-family: "Space Grotesk", sans-serif !important;
        font-size: 12px !important;
        font-weight: 600 !important;
        color: var(--text-secondary) !important;
        cursor: pointer;
        transition: border-color 0.15s, background 0.15s;
        min-width: 100px;
        background-image: url("data:image/svg+xml,%3Csvg xmlns=%27http://www.w3.org/2000/svg%27 width=%2710%27 height=%2710%27 fill=%27%2394a3b8%27 viewBox=%270 0 16 16%27%3E%3Cpath d=%27M8 11L3 6h10l-5 5z%27/%3E%3C/svg%3E") !important;
        background-repeat: no-repeat !important;
        background-position: right 12px center !important;
      }
      .theme-select:hover {
        border-color: var(--border-accent) !important;
        background: var(--bg-glass-hover) !important;
      }
      .theme-select:focus {
        border-color: var(--accent-primary) !important;
        box-shadow: 0 0 0 3px rgba(15,118,110,0.15) !important;
        outline: none !important;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         BUTTONS
         ═══════════════════════════════════════════════════════════════════════════ */
      .btn-icon {
        width: 42px;
        height: 42px;
        border-radius: var(--radius-md);
        border: 1px solid var(--border-subtle);
        background: var(--bg-glass);
        color: var(--text-secondary);
        display: inline-flex;
        align-items: center;
        justify-content: center;
        cursor: pointer;
        transition: background 0.15s ease, border-color 0.15s ease, color 0.15s ease;
        font-size: 16px;
      }
      
      .btn-icon:hover {
        background: rgba(99, 102, 241, 0.15);
        border-color: var(--border-accent);
        color: var(--accent-primary);
      }
      
      .btn-primary {
        padding: 12px 24px;
        border-radius: var(--radius-md);
        border: none;
        background: var(--gradient-primary);
        color: white;
        font-weight: 600;
        font-size: 14px;
        cursor: pointer;
        transition: opacity 0.15s ease, transform 0.15s ease;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        gap: 10px;
        font-family: inherit;
        position: relative;
        overflow: hidden;
        letter-spacing: 0.3px;
      }
      
      .btn-primary::before {
        content: "";
        position: absolute;
        inset: 0;
        background: linear-gradient(135deg, rgba(255,255,255,0.2) 0%, transparent 50%);
        opacity: 0;
        transition: opacity var(--transition-normal);
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 16px rgba(99, 102, 241, 0.4), 0 8px 32px rgba(99, 102, 241, 0.2);
      }
      
      .btn-primary:hover::before { opacity: 1; }
      
      .btn-primary:active { transform: translateY(0); }
      
      .btn-secondary {
        padding: 10px 20px;
        border-radius: var(--radius-sm);
        border: 1px solid var(--border-subtle);
        background: var(--bg-glass);
        color: var(--text-secondary);
        font-weight: 500;
        font-size: 13px;
        cursor: pointer;
        transition: background 0.15s ease, color 0.15s ease;
        display: inline-flex;
        align-items: center;
        gap: 8px;
        font-family: inherit;
        letter-spacing: 0.2px;
      }
      
      .btn-secondary:hover {
        background: rgba(99, 102, 241, 0.1);
        border-color: var(--border-accent);
        color: var(--accent-primary);
        transform: translateY(-1px);
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
      }
      
      .btn-success {
        background: var(--gradient-secondary) !important;
      }
      
      .btn-success:hover {
        box-shadow: 0 0 30px rgba(16, 185, 129, 0.4) !important;
      }
      
      .btn-sm {
        padding: 8px 14px;
        font-size: 12px;
        border-radius: var(--radius-xs);
      }
      
      .btn-block {
        width: 100%;
        justify-content: center;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         MAIN LAYOUT
         ═══════════════════════════════════════════════════════════════════════════ */
      .main-container {
        display: grid;
        grid-template-columns: 300px 1fr;
        gap: 24px;
        padding: 24px 32px;
        min-height: calc(100vh - 100px);
        transition: grid-template-columns 0.28s ease;
        overflow-x: hidden;
      }

      .main-container.collapsed-sidebar {
        grid-template-columns: 0 1fr !important;
      }

      .main-container.collapsed-sidebar .sidebar {
        display: none;
      }
      
      @media (max-width: 1200px) {
        .main-container {
          grid-template-columns: 1fr;
          padding: 16px;
        }
      }

      .tab-content {
        overflow-x: hidden;
      }

      /* Prevent Bootstrap row negative margins from spilling into the left gutter */
      .main-container .row {
        margin-left: 0;
        margin-right: 0;
      }

      .main-container .row > [class^="col-"],
      .main-container .row > [class*=" col-"] {
        padding-left: 12px;
        padding-right: 12px;
        min-width: 0;
      }

      .main-container .row > [class^="col-"]:first-child,
      .main-container .row > [class*=" col-"]:first-child {
        padding-left: 0;
      }

      .main-container .row > [class^="col-"]:last-child,
      .main-container .row > [class*=" col-"]:last-child {
        padding-right: 0;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         GLASS CARDS — FROSTED GLASS PANELS
         ═══════════════════════════════════════════════════════════════════════════ */
      .glass-card {
        position: relative;
        background: var(--bg-card);
        border: 1px solid var(--border-subtle);
        border-radius: var(--radius-xl);
        overflow: hidden;
        /* Frosted glass effect */
        backdrop-filter: blur(var(--blur-md));
        -webkit-backdrop-filter: blur(var(--blur-md));
        box-shadow: var(--shadow-glass);
        transition: box-shadow 0.25s ease, border-color 0.25s ease, transform 0.25s ease;
      }
      
      .glass-card::before {
        content: "";
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 1px;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.4), transparent);
      }
      
      .glass-card:hover {
        border-color: var(--border-accent);
        box-shadow: var(--shadow-glass), 0 0 30px rgba(99, 102, 241, 0.1);
        transform: translateY(-2px);
      }
      
      .card-header {
        padding: 18px 20px;
        border-bottom: 1px solid var(--border-subtle);
        background: var(--gradient-surface);
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      
      .card-title {
        display: flex;
        align-items: center;
        gap: 12px;
        font-size: 15px;
        font-weight: 600;
        color: var(--text-primary);
        margin: 0;
      }
      
      .card-title i {
        font-size: 18px;
        color: var(--accent-primary);
      }
      
      .card-body {
        padding: 20px;
      }

      .card-footer {
        padding: 14px 20px;
        border-top: 1px solid var(--border-subtle);
        background: var(--surface-card-footer);
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         ENTERPRISE UI ENHANCEMENTS — GLASSMORPHISM
         ═══════════════════════════════════════════════════════════════════════════ */
      .nav-tabs {
        border-bottom: 1px solid var(--border-subtle);
        background: var(--surface-nav);
        backdrop-filter: blur(var(--blur-sm));
        -webkit-backdrop-filter: blur(var(--blur-sm));
        padding: 6px 10px;
        border-radius: var(--radius-md);
        box-shadow: var(--shadow-glass);
      }

      .nav-tabs > li > a {
        border: 0;
        color: var(--text-secondary);
        font-weight: 600;
        padding: 10px 14px;
        border-radius: var(--radius-sm);
        transition: color 0.15s ease, background 0.15s ease;
      }

      .nav-tabs > li > a:hover {
        color: var(--accent-primary);
        background: rgba(99, 102, 241, 0.1);
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: #fff;
        background: var(--gradient-primary);
        box-shadow: 0 4px 16px rgba(99, 102, 241, 0.35);
      }

      .glass-card,
      .sidebar-section {
        border: 1px solid var(--border-subtle);
        backdrop-filter: blur(var(--blur-md));
        -webkit-backdrop-filter: blur(var(--blur-md));
      }

      .card-header {
        background: var(--surface-card-header);
      }

      .card-title {
        letter-spacing: 0.2px;
      }

      .form-control,
      select,
      input[type="text"],
      input[type="number"],
      input[type="password"],
      textarea {
        background: var(--surface-input) !important;
        border: 1px solid var(--border-subtle) !important;
        color: var(--text-primary) !important;
        border-radius: var(--radius-sm) !important;
        transition: border-color 0.2s ease, box-shadow 0.2s ease;
      }

      .form-control:focus,
      select:focus,
      input:focus,
      textarea:focus {
        border-color: var(--accent-primary) !important;
        box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.15) !important;
      }

      .btn-primary {
        background: var(--gradient-primary) !important;
        border: 0 !important;
        box-shadow: 0 4px 16px rgba(99, 102, 241, 0.3);
      }

      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 24px rgba(99, 102, 241, 0.45);
      }

      .btn-secondary {
        background: var(--surface-control) !important;
        border: 1px solid var(--border-subtle) !important;
      }

      .btn-secondary:hover {
        background: rgba(99, 102, 241, 0.1) !important;
        border-color: var(--border-accent) !important;
      }

      .btn, .btn-primary, .btn-secondary, .btn-success, .btn-warning, .btn-danger {
        transition: transform var(--transition-fast), box-shadow var(--transition-normal);
      }

      .btn:active {
        transform: translateY(1px) scale(0.99);
      }

      .status-badge {
        background: var(--surface-badge);
        border: 1px solid var(--border-subtle);
        border-radius: var(--radius-full);
        padding: 6px 10px;
        display: inline-flex;
        gap: 8px;
        align-items: center;
        backdrop-filter: blur(var(--blur-sm));
        -webkit-backdrop-filter: blur(var(--blur-sm));
      }

      .chip {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 4px 10px;
        border-radius: var(--radius-full);
        background: rgba(99, 102, 241, 0.12);
        color: var(--text-secondary);
        border: 1px solid rgba(99, 102, 241, 0.2);
        font-size: 12px;
        font-weight: 600;
      }

      .table, .dataTable {
        border-color: var(--border-subtle) !important;
      }

      .table thead th, .dataTable thead th {
        background: var(--surface-table-head) !important;
        color: var(--text-secondary) !important;
        border-bottom: 1px solid var(--border-accent) !important;
        text-transform: uppercase;
        letter-spacing: 0.6px;
        font-size: 11px;
      }

      .table tbody tr:hover, .dataTable tbody tr:hover {
        background: rgba(99, 102, 241, 0.06) !important;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: var(--text-secondary) !important;
        background: transparent !important;
        border: 1px solid var(--border-subtle) !important;
        border-radius: var(--radius-sm) !important;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: rgba(99, 102, 241, 0.15) !important;
        color: var(--accent-primary) !important;
        border-color: var(--border-accent) !important;
      }

      .modal-content {
        background: var(--surface-modal);
        border: 1px solid var(--border-subtle);
        border-radius: var(--radius-xl);
        box-shadow: var(--shadow-lg), 0 0 60px rgba(99, 102, 241, 0.1);
        backdrop-filter: blur(var(--blur-lg));
        -webkit-backdrop-filter: blur(var(--blur-lg));
      }

      .modal-header {
        border-bottom: 1px solid var(--border-subtle);
      }

      .modal-footer {
        border-top: 1px solid var(--border-subtle);
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         SIDEBAR — FROSTED GLASS
         ═══════════════════════════════════════════════════════════════════════════ */
      .sidebar {
        display: flex;
        flex-direction: column;
        gap: 20px;
        min-width: 0;
      }
      
      .sidebar-section {
        background: var(--bg-card);
        border: 1px solid var(--border-subtle);
        border-radius: var(--radius-lg);
        padding: 20px;
        /* Frosted glass effect */
        backdrop-filter: blur(var(--blur-md));
        -webkit-backdrop-filter: blur(var(--blur-md));
        box-shadow: var(--shadow-glass);
        transition: border-color 0.25s ease, box-shadow 0.25s ease, transform 0.25s ease;
        overflow: hidden;
        box-sizing: border-box;
        position: relative;
      }

      .sidebar-section::before {
        content: "";
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 1px;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.3), transparent);
      }

      .sidebar-section:hover {
        border-color: var(--border-accent);
        box-shadow: var(--shadow-glass), 0 0 25px rgba(99, 102, 241, 0.08);
        transform: translateY(-2px);
      }
      
      .sidebar-section * {
        box-sizing: border-box;
      }
      
      .sidebar-section .form-group,
      .sidebar-section .checkbox,
      .sidebar-section .shiny-input-container {
        margin-left: 0;
        margin-right: 0;
        padding-left: 0;
        max-width: 100%;
      }
      
      .eda-options-panel {
        padding-left: 22px;
        padding-right: 16px;
      }

      .eda-options-panel h6 {
        margin-left: 2px;
        margin-top: 4px;
        margin-bottom: 10px;
      }

      .eda-options-panel .shiny-input-checkboxgroup {
        padding-left: 4px;
      }

      .eda-options-panel .checkbox {
        margin-left: 2px;
        margin-right: 2px;
      }
      
      .section-title {
        display: flex;
        align-items: center;
        gap: 10px;
        font-size: 11px;
        font-weight: 700;
        color: var(--accent-primary);
        text-transform: uppercase;
        letter-spacing: 1.2px;
        margin-bottom: 14px;
        padding-bottom: 10px;
        border-bottom: 1px solid var(--border-subtle);
      }

      /* Focus outlines for keyboard navigation */
      .btn-icon:focus, .btn-primary:focus, .btn-secondary:focus, select:focus, input:focus, textarea:focus {
        outline: 3px solid rgba(99, 102, 241, 0.12);
        outline-offset: 2px;
      }
      
      .section-title i {
        font-size: 14px;
        opacity: 0.9;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         FORM ELEMENTS
         ═══════════════════════════════════════════════════════════════════════════ */
      .form-group { margin-bottom: 16px !important; }
      .form-group:last-child { margin-bottom: 0 !important; }
      
      label.control-label {
        font-size: 12px;
        font-weight: 500;
        color: var(--text-secondary);
        margin-bottom: 8px;
        display: block;
      }
      
      select, 
      input[type="text"], 
      input[type="number"],
      input[type="password"],
      textarea {
        width: 100%;
        padding: 12px 16px;
        background: var(--bg-tertiary) !important;
        border: 1px solid var(--border-subtle) !important;
        border-radius: var(--radius-sm) !important;
        color: var(--text-primary) !important;
        font-family: inherit !important;
        font-size: 13px !important;
        transition: border-color 0.15s ease;
      }
      
      select:focus, 
      input[type="text"]:focus, 
      input[type="number"]:focus,
      input[type="password"]:focus,
      textarea:focus {
        border-color: var(--accent-primary) !important;
        outline: none !important;
        box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.15) !important;
      }
      
      select {
        appearance: none;
        background-image: url("data:image/svg+xml,%3Csvg xmlns=\'http://www.w3.org/2000/svg\' width=\'12\' height=\'12\' fill=\'%2394a3b8\' viewBox=\'0 0 16 16\'%3E%3Cpath d=\'M8 11L3 6h10l-5 5z\'/%3E%3C/svg%3E") !important;
        background-repeat: no-repeat !important;
        background-position: right 14px center !important;
        padding-right: 40px !important;
      }
      
      select option {
        background: var(--bg-tertiary);
        color: var(--text-primary);
        padding: 10px;
      }
      
      .shiny-input-container { width: 100% !important; }
      
      /* File Input */
      .shiny-input-container input[type="file"] {
        display: none;
      }
      
      .input-group-btn .btn {
        background: var(--bg-tertiary);
        border: 1px solid var(--border-subtle);
        color: var(--text-secondary);
        padding: 12px 16px;
        border-radius: var(--radius-sm);
        font-size: 13px;
        cursor: pointer;
        transition: background 0.15s ease, color 0.15s ease;
      }
      
      .input-group-btn .btn:hover {
        background: rgba(99, 102, 241, 0.1);
        border-color: var(--border-accent);
        color: var(--accent-primary);
      }
      
      /* Checkbox */
      .checkbox,
      .shiny-input-checkboxgroup .checkbox {
        margin-left: 0 !important;
        padding-left: 0 !important;
      }

      .checkbox label, .shiny-input-checkbox-group label {
        display: flex;
        align-items: center;
        gap: 10px;
        font-size: 13px;
        color: var(--text-secondary);
        cursor: pointer;
        padding: 8px 0;
        margin-left: 0 !important;
        padding-left: 0 !important;
        overflow: visible;
      }
      
      .checkbox input[type="checkbox"],
      .shiny-input-checkbox-group input[type="checkbox"] {
        width: 18px;
        height: 18px;
        accent-color: var(--accent-primary);
        cursor: pointer;
        margin: 0 10px 0 0 !important;
        position: relative;
      }
      
      /* Slider */
      .irs--shiny {
        height: 40px;
      }

      .irs--shiny .irs-line {
        height: 6px;
        background: var(--bg-tertiary);
        border-radius: 999px;
        border: 1px solid var(--border-subtle);
      }

      .irs--shiny .irs-bar {
        height: 6px;
        background: var(--gradient-primary);
        border: none;
        border-radius: 999px;
      }

      .irs--shiny .irs-handle {
        top: 18px;
        width: 18px;
        height: 18px;
        background: #fff;
        border: 2px solid var(--accent-primary);
        box-shadow: 0 4px 12px rgba(0, 0, 0, 0.18);
      }

      .irs--shiny .irs-handle:hover,
      .irs--shiny .irs-handle.state_hover {
        box-shadow: 0 6px 16px rgba(99, 102, 241, 0.35);
      }

      .irs--shiny .irs-single,
      .irs--shiny .irs-min,
      .irs--shiny .irs-max {
        background: var(--surface-control);
        color: var(--text-primary);
        border: 1px solid var(--border-subtle);
        border-radius: 999px;
        font-size: 11px;
        padding: 2px 8px;
      }

      .irs--shiny .irs-grid-text {
        color: var(--text-dim);
        font-size: 10px;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         TABS
         ═══════════════════════════════════════════════════════════════════════════ */
      .nav-tabs {
        border: none !important;
        background: var(--surface-nav);
        border-radius: var(--radius-xl) var(--radius-xl) 0 0;
        padding: 4px 16px 0;
        display: flex;
        gap: 4px;
        flex-wrap: nowrap;
        overflow-x: auto;
        overflow-y: hidden;
        -webkit-overflow-scrolling: touch;
        scrollbar-width: thin;
        scrollbar-color: rgba(99, 102, 241, 0.35) transparent;
        position: relative;
        z-index: auto;
        box-shadow: none;
      }

      .nav-tabs::-webkit-scrollbar {
        height: 6px;
      }

      .nav-tabs::-webkit-scrollbar-thumb {
        background: rgba(99, 102, 241, 0.35);
        border-radius: 999px;
      }
      
      .nav-tabs > li {
        margin: 0 !important;
        flex: 0 0 auto;
      }
      
      .nav-tabs > li > a {
        border: none !important;
        border-radius: var(--radius-md) var(--radius-md) 0 0 !important;
        padding: 12px 20px !important;
        color: var(--text-muted) !important;
        font-weight: 600 !important;
        font-size: 13px !important;
        background: transparent !important;
        transition: color 0.15s ease, background 0.15s ease !important;
        display: flex !important;
        align-items: center !important;
        gap: 8px !important;
        position: relative !important;
        letter-spacing: 0.2px !important;
      }
      
      .nav-tabs > li > a:hover {
        color: var(--text-primary) !important;
        background: rgba(99, 102, 241, 0.08) !important;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: var(--accent-primary) !important;
        background: var(--bg-card) !important;
        border: 1px solid rgba(99, 102, 241, 0.15) !important;
        border-bottom: 1px solid var(--bg-card) !important;
        margin-bottom: -1px;
        position: relative;
        box-shadow: 0 -2px 12px rgba(99, 102, 241, 0.08);
      }
      
      .nav-tabs > li.active > a::before {
        content: "";
        position: absolute;
        top: 0;
        left: 20%;
        right: 20%;
        height: 2px;
        background: var(--gradient-primary);
        border-radius: 0 0 4px 4px;
      }
      
      .nav-tabs > li.active > a i {
        color: var(--accent-primary) !important;
        filter: drop-shadow(0 0 4px rgba(99, 102, 241, 0.4));
      }
      
      .tab-content {
        background: var(--bg-card);
        border: 1px solid rgba(255, 255, 255, 0.06);
        border-top: none;
        border-radius: 0 0 var(--radius-xl) var(--radius-xl);
        padding: 28px;
        min-height: 500px;
        box-shadow: 0 8px 24px rgba(0, 0, 0, 0.1);
        overflow: hidden;
      }

      .tab-content .row {
        margin-left: 0;
        margin-right: 0;
      }

      .tab-content [class^="col-"] {
        padding-left: 12px;
        padding-right: 12px;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         STAT CARDS
         ═══════════════════════════════════════════════════════════════════════════ */
      .stat-cards {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
        gap: 16px;
        margin-bottom: 32px;
      }
      
      .stat-card {
        background: var(--bg-card);
        border: 1px solid rgba(255, 255, 255, 0.08);
        border-radius: var(--radius-lg);
        padding: 22px 18px;
        text-align: center;
        position: relative;
        overflow: hidden;
        transition: transform 0.2s ease, box-shadow 0.2s ease, border-color 0.2s ease;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.04), 0 4px 12px rgba(0, 0, 0, 0.06);
      }
      
      .stat-card:hover {
        border-color: rgba(99, 102, 241, 0.2);
        transform: translateY(-2px);
        box-shadow: 0 4px 16px rgba(0, 0, 0, 0.12);
      }
      
      .stat-card .value {
        font-size: 30px;
        font-weight: 800;
        background: var(--gradient-primary);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        line-height: 1.2;
        letter-spacing: -0.5px;
      }
      
      .stat-card .label {
        font-size: 11px;
        color: var(--text-muted);
        text-transform: uppercase;
        letter-spacing: 1px;
        margin-top: 8px;
        font-weight: 600;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         VARIABLE TAGS
         ═══════════════════════════════════════════════════════════════════════════ */
      .variable-list {
        display: flex;
        flex-wrap: wrap;
        gap: 8px;
      }
      
      .variable-tag {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 6px 12px;
        border-radius: var(--radius-full);
        font-size: 12px;
        font-weight: 600;
        cursor: default;
        letter-spacing: 0.2px;
      }
      
      .variable-tag.numeric {
        background: rgba(16, 185, 129, 0.1);
        color: #34d399;
        border: 1px solid rgba(16, 185, 129, 0.2);
      }
      
      .variable-tag.categorical {
        background: rgba(245, 158, 11, 0.1);
        color: #fbbf24;
        border: 1px solid rgba(245, 158, 11, 0.2);
      }
      
      .variable-tag.datetime {
        background: rgba(236, 72, 153, 0.1);
        color: #f472b6;
        border: 1px solid rgba(236, 72, 153, 0.2);
      }
      
      .variable-tag i { font-size: 10px; opacity: 0.8; }

      /* ═══════════════════════════════════════════════════════════════════════════
         CHART CONTAINER
         ═══════════════════════════════════════════════════════════════════════════ */

      /* Compact layout adjustments */
      .compact-layout .main-container { padding: 12px 16px; gap: 12px; }
      .compact-layout .sidebar { width: 260px; }
      .compact-layout .sidebar-section { padding: 12px; }
      .compact-layout .chart-container { padding: 12px 14px 10px; }
      .compact-layout .stat-card .value { font-size: 22px; }
      .compact-layout .card-header, .compact-layout .card-body { padding: 8px 12px; }
      .compact-layout .chart-title { font-size: 14px; }
      .compact-layout .section-title { font-size: 11px; }

      .chart-container {
        background: var(--bg-card);
        border: 1px solid rgba(255, 255, 255, 0.08);
        border-radius: var(--radius-lg);
        padding: 22px 24px 18px;
        margin-bottom: 20px;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.04), 0 4px 12px rgba(0, 0, 0, 0.06);
        transition: border-color 0.2s ease, box-shadow 0.2s ease;
        overflow: hidden;
      }
      
      .chart-container:hover {
        border-color: rgba(99, 102, 241, 0.15);
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.06), 0 8px 24px rgba(0, 0, 0, 0.1);
      }

      /* Dashboard grid styling */
      .dashboard-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 20px;
      }

      .dashboard-panel {
        background: var(--bg-card);
        border: 1px solid rgba(255, 255, 255, 0.08);
        border-radius: var(--radius-lg);
        padding: 18px;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.04), 0 4px 12px rgba(0, 0, 0, 0.06);
        min-height: 140px;
        transition: border-color 0.2s ease, box-shadow 0.2s ease;
      }

      .dashboard-panel:hover {
        border-color: rgba(99, 102, 241, 0.12);
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.06), 0 6px 20px rgba(0, 0, 0, 0.08);
      }
      
      .chart-header {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        margin-bottom: 14px;
      }

      .chart-title {
        display: inline-flex;
        align-items: center;
        gap: 10px;
        font-size: 15px;
        font-weight: 700;
        color: var(--text-primary);
        margin: 0;
        letter-spacing: 0.1px;
      }

      .chart-title i {
        color: var(--accent-primary);
        font-size: 16px;
        filter: drop-shadow(0 0 3px rgba(99, 102, 241, 0.3));
      }

      .btn-icon {
        background: var(--bg-glass);
        border: 1px solid var(--border-subtle);
        color: var(--text-secondary);
        border-radius: var(--radius-sm);
        padding: 6px 10px;
        transition: color 0.15s ease, border-color 0.15s ease;
      }

      .btn-icon:hover {
        color: var(--text-primary);
        border-color: var(--border-accent);
      }
      
      .chart-main {
        min-height: 600px;
        position: relative;
      }
      
      .chart-plot-area {
        width: 100%;
        height: 600px;
        position: relative;
        overflow: hidden;
        border-radius: var(--radius-md);
        background: var(--bg-tertiary);
      }
      
      .chart-plot-area .plotly {
        width: 100% !important;
        height: 100% !important;
      }

      /* Shiny busy / recalculating indicators */
      .recalculating { opacity: 0.45; transition: opacity 0.3s ease; }
      html.shiny-busy .app-header::after {
        animation: busyBar 1.2s ease-in-out infinite;
        opacity: 1 !important;
      }
      @keyframes busyBar {
        0%   { background-position: -200% 0; }
        100% { background-position: 200% 0; }
      }
      html.shiny-busy .app-header::after {
        background: linear-gradient(90deg,
          var(--accent-primary),
          var(--accent-secondary),
          var(--accent-success),
          var(--accent-info),
          var(--accent-secondary),
          var(--accent-primary));
        background-size: 200% 100%;
      }
      html.shiny-busy .loading-text::after { content: "..."; }

      /* ═══════════════════════════════════════════════════════════════════════════
         FULLSCREEN MODAL
         ═══════════════════════════════════════════════════════════════════════════ */
      .modal-fullscreen .modal-dialog {
        width: 100vw !important;
        max-width: 100vw !important;
        height: 100vh;
        margin: 0;
      }

      .modal-fullscreen .modal-content {
        height: 100vh;
        border-radius: 0;
        background: var(--bg-secondary);
        border: 0;
        box-shadow: none;
      }

      .modal-fullscreen .modal-body {
        height: calc(100vh - 120px);
        overflow: hidden;
      }

      .modal-fullscreen .plotly {
        width: 100% !important;
        height: 100% !important;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         CHAT & QUERY BOXES
         ═══════════════════════════════════════════════════════════════════════════ */
      .chat-layout {
        display: flex;
        align-items: stretch;
        min-height: calc(100vh - 200px);
      }
      
      .chat-layout > [class^="col-"] {
        display: flex;
        flex-direction: column;
      }
      
      .chat-main {
        display: flex;
        flex-direction: column;
        flex: 1 1 auto;
        min-height: calc(100vh - 200px);
      }
      
      /* Ensure chat input always anchored at bottom */
      .chat-main .form-group textarea {
        border: 2px solid var(--border-subtle);
        border-radius: var(--radius-lg);
        background: var(--bg-glass);
        color: var(--text-primary);
        font-size: 15px;
        line-height: 1.6;
        padding: 16px 20px;
        resize: none;
        transition: border-color 0.2s ease, box-shadow 0.2s ease;
        min-height: 60px;
      }

      .chat-main .form-group textarea:focus {
        border-color: var(--accent-primary);
        box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.1);
        outline: none;
      }
      .chat-container {
        flex: 1 1 auto;
        min-height: 520px;
        overflow-y: auto;
        background: var(--bg-tertiary);
        border: 1px solid var(--border-subtle);
        border-radius: var(--radius-lg);
        padding: 24px;
        margin-bottom: 16px;
        background-image: 
          radial-gradient(circle at 20% 80%, rgba(99, 102, 241, 0.03) 0%, transparent 50%),
          radial-gradient(circle at 80% 20%, rgba(16, 185, 129, 0.03) 0%, transparent 50%);
      }
      
      .message {
        max-width: 85%;
        padding: 16px 22px;
        border-radius: var(--radius-xl);
        margin-bottom: 18px;
        font-size: 15px;
        line-height: 1.7;
        animation: slideIn 0.4s ease;
        box-shadow: 0 3px 12px rgba(0, 0, 0, 0.1);
        position: relative;
        word-wrap: break-word;
        overflow-wrap: break-word;
      }

      .message::before {
        content: "";
        position: absolute;
        bottom: -8px;
        width: 0;
        height: 0;
        border-style: solid;
      }

      .message.user::before {
        right: 20px;
        border-width: 8px 8px 0 8px;
        border-color: var(--accent-primary) transparent transparent transparent;
      }

      .message.assistant::before {
        left: 20px;
        border-width: 8px 8px 0 8px;
        border-color: var(--bg-glass) transparent transparent transparent;
      }
      
      @keyframes slideIn {
        from { opacity: 0; transform: translateY(10px); }
        to { opacity: 1; transform: translateY(0); }
      }
      
      .message.user {
        margin-left: auto;
        background: var(--gradient-primary);
        color: white;
        border-bottom-right-radius: 4px;
      }
      
      .message.assistant {
        margin-right: auto;
        background: var(--bg-glass);
        border: 1px solid var(--border-subtle);
        color: var(--text-primary);
        border-bottom-left-radius: 4px;
      }

      /* Chat bubble styling */
      .chat-bubble {
        padding: 12px 18px;
        border-radius: var(--radius-lg);
        max-width: 78%;
        white-space: pre-wrap;
        font-size: 14px;
        line-height: 1.65;
        animation: slideIn 0.25s ease;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
      }

      .chat-bubble.chat-user {
        background: var(--gradient-primary);
        color: white;
        border-bottom-right-radius: 4px;
      }

      .chat-bubble.chat-ai {
        background: var(--bg-glass);
        border: 1px solid var(--border-subtle);
        color: var(--text-primary);
        border-bottom-left-radius: 4px;
      }
      
      .query-box {
        background: rgba(99, 102, 241, 0.04);
        border: 1px solid rgba(99, 102, 241, 0.12);
        border-radius: var(--radius-lg);
        padding: 24px;
        margin-bottom: 20px;
      }

      .query-history-empty,
      .query-result-empty {
        display: flex;
        align-items: center;
        gap: 10px;
        padding: 10px 12px;
        font-size: 12px;
        color: var(--text-muted);
        background: var(--bg-glass);
        border: 1px dashed var(--border-subtle);
        border-radius: var(--radius-md);
      }

      .query-history-item {
        padding: 10px 12px;
        border: 1px solid var(--border-subtle);
        border-radius: var(--radius-md);
        background: var(--bg-glass);
        margin-bottom: 8px;
      }

      .query-history-time {
        font-size: 11px;
        color: var(--text-dim);
      }
      
      .response-box {
        background: var(--bg-tertiary);
        border: 1px solid var(--border-subtle);
        border-radius: var(--radius-lg);
        padding: 24px;
        margin-bottom: 20px;
      }
      
      .code-block {
        background: var(--bg-primary) !important;
        border: 1px solid var(--border-subtle);
        border-radius: var(--radius-md);
        padding: 16px 20px !important;
        font-family: "JetBrains Mono", "Fira Code", monospace !important;
        font-size: 13px !important;
        color: var(--accent-success) !important;
        overflow-x: auto;
        margin: 0;
        line-height: 1.7;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         DATA TABLE
         ═══════════════════════════════════════════════════════════════════════════ */
      .dataTables_wrapper {
        background: transparent !important;
      }
      
      table.dataTable {
        border-collapse: collapse !important;
        background: var(--bg-tertiary) !important;
        border-radius: var(--radius-lg) !important;
        overflow: hidden;
        box-shadow: 0 1px 4px rgba(0, 0, 0, 0.06);
      }
      
      table.dataTable thead th {
        background: var(--surface-table-head) !important;
        color: var(--text-secondary) !important;
        font-weight: 700 !important;
        font-size: 11px !important;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        padding: 14px 16px !important;
        border-bottom: 2px solid var(--border-accent) !important;
      }
      
      table.dataTable tbody td {
        background: transparent !important;
        color: var(--text-secondary) !important;
        padding: 12px 16px !important;
        border-bottom: 1px solid var(--border-subtle) !important;
        font-size: 13px;
      }
      
      table.dataTable tbody tr:hover td {
        background: rgba(99, 102, 241, 0.05) !important;
      }
      
      .dataTables_filter input,
      .dataTables_length select {
        background: var(--bg-tertiary) !important;
        border: 1px solid var(--border-subtle) !important;
        border-radius: var(--radius-sm) !important;
        color: var(--text-primary) !important;
        padding: 8px 12px !important;
      }
      
      .dataTables_info,
      .dataTables_length label,
      .dataTables_filter label {
        color: var(--text-muted) !important;
        font-size: 12px !important;
      }
      
      .dataTables_paginate .paginate_button {
        background: var(--bg-glass) !important;
        border: 1px solid var(--border-subtle) !important;
        border-radius: var(--radius-xs) !important;
        color: var(--text-secondary) !important;
        padding: 6px 12px !important;
        margin: 0 2px !important;
        transition: background 0.15s ease, border-color 0.15s ease !important;
      }
      
      .dataTables_paginate .paginate_button:hover {
        background: rgba(99, 102, 241, 0.1) !important;
        border-color: var(--border-accent) !important;
        color: var(--accent-primary) !important;
      }
      
      .dataTables_paginate .paginate_button.current {
        background: var(--accent-primary) !important;
        border-color: var(--accent-primary) !important;
        color: white !important;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         PLOTLY OVERRIDES
         ═══════════════════════════════════════════════════════════════════════════ */
      .plotly {
        border-radius: var(--radius-md);
        overflow: hidden;
      }
      
      .js-plotly-plot .plotly .modebar {
        background: var(--bg-secondary) !important;
        border-radius: var(--radius-sm);
        padding: 4px;
      }
      
      .js-plotly-plot .plotly .modebar-btn {
        color: var(--text-muted) !important;
      }
      
      .js-plotly-plot .plotly .modebar-btn:hover {
        color: var(--accent-primary) !important;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         LOADING STATE
         ═══════════════════════════════════════════════════════════════════════════ */
      .loading-overlay {
        position: fixed;
        inset: 0;
        background: rgba(10, 10, 15, 0.92);
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        z-index: 9999;
      }
      
      .loading-spinner {
        width: 48px;
        height: 48px;
        border: 3px solid var(--border-subtle);
        border-top-color: var(--accent-primary);
        border-right-color: var(--accent-secondary);
        border-radius: 50%;
        animation: spin 0.8s linear infinite;
        box-shadow: 0 0 20px rgba(99, 102, 241, 0.2);
      }
      
      @keyframes spin {
        to { transform: rotate(360deg); }
      }
      
      .loading-text {
        margin-top: 20px;
        font-size: 14px;
        color: var(--text-secondary);
        font-weight: 500;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         FOOTER
         ═══════════════════════════════════════════════════════════════════════════ */
      .app-footer {
        padding: 16px 32px;
        text-align: center;
        color: var(--text-muted);
        font-size: 12px;
        border-top: 1px solid var(--border-subtle);
        background: var(--surface-card-footer);
        letter-spacing: 0.3px;
      }
      .footer-inner {
        display: inline-flex;
        align-items: center;
        gap: 10px;
        flex-wrap: wrap;
        justify-content: center;
      }
      .footer-text {
        font-weight: 600;
        color: var(--text-secondary);
      }
      .footer-sep {
        width: 4px;
        height: 4px;
        border-radius: 50%;
        background: var(--text-dim);
        opacity: 0.5;
        flex-shrink: 0;
      }
      .app-footer a {
        color: var(--accent-primary);
        text-decoration: none;
        transition: color var(--transition-fast);
        font-weight: 500;
        display: inline-flex;
        align-items: center;
      }
      .app-footer a:hover {
        color: var(--accent-secondary);
      }

      hr {
        border: 0;
        height: 1px;
        background: linear-gradient(90deg, transparent, var(--border-subtle), transparent);
        margin: 14px 0;
      }


      /* ═══════════════════════════════════════════════════════════════════════════
         UTILITY CLASSES
         ═══════════════════════════════════════════════════════════════════════════ */
      .text-gradient {
        background: var(--gradient-primary);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
      }
      
      .mt-2 { margin-top: 8px !important; }
      .mt-3 { margin-top: 12px !important; }
      .mt-4 { margin-top: 16px !important; }
      .mb-2 { margin-bottom: 8px !important; }
      .mb-3 { margin-bottom: 12px !important; }
      .mb-4 { margin-bottom: 16px !important; }
      .gap-2 { gap: 8px; }
      .gap-3 { gap: 12px; }
      
      hr {
        border: none;
        border-top: 1px solid var(--border-subtle);
        margin: 20px 0;
      }

      /* Hide default Shiny notification styling */
      .shiny-notification {
        background: var(--bg-secondary) !important;
        border: 1px solid var(--border-accent) !important;
        border-radius: var(--radius-md) !important;
        color: var(--text-primary) !important;
        box-shadow: var(--shadow-lg) !important;
      }

      /* Fullscreen modal for chart popouts */
      .modal-fullscreen .modal-dialog {
        width: 100vw !important;
        max-width: 100vw !important;
        height: 100vh !important;
        margin: 0 !important;
        padding: 0 !important;
      }
      .modal-fullscreen .modal-content {
        height: 100vh !important;
        border-radius: 0 !important;
      }
      .modal-fullscreen .modal-body {
        height: calc(100vh - 56px) !important;
        overflow: hidden !important;
      }
      .modal-fullscreen .plotly.html-widget {
        height: 100% !important;
      }
      .btn-fullscreen {
        background: rgba(99, 102, 241, 0.15);
        border: 1px solid rgba(99, 102, 241, 0.5);
        color: var(--text-primary);
        border-radius: var(--radius-sm);
        padding: 6px 12px;
        transition: background 0.15s ease;
      }

      .btn-fullscreen:hover {
        background: rgba(99, 102, 241, 0.25);
      }

      /* JS fallback fullscreen mode when native API is blocked/unavailable */
      body.dep-fs-lock {
        overflow: hidden !important;
      }

      .modal.dep-fs-fallback {
        display: block !important;
        padding: 0 !important;
        background: rgba(2, 6, 23, 0.92);
        z-index: 2050;
      }

      .modal.dep-fs-fallback .modal-dialog {
        width: 100vw !important;
        max-width: 100vw !important;
        height: 100vh !important;
        margin: 0 !important;
        padding: 0 !important;
      }

      .modal.dep-fs-fallback .modal-content {
        height: 100vh !important;
        border-radius: 0 !important;
      }

      .modal.dep-fs-fallback .modal-body {
        height: calc(100vh - 56px) !important;
        overflow: hidden !important;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         PROFESSIONAL POLISH: TRANSITIONS, FOCUS RINGS, SELECTIONS
         ═══════════════════════════════════════════════════════════════════════════ */
      ::selection {
        background: rgba(99, 102, 241, 0.3);
        color: var(--text-primary);
      }

      /* Smooth focus rings for accessibility */
      :focus-visible {
        outline: 2px solid var(--accent-primary);
        outline-offset: 2px;
      }

      /* Subtle text shadow on headings */
      h5, .chart-title, .section-title {
        text-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
      }

      /* Better textarea styling */
      textarea.form-control {
        resize: vertical;
        min-height: 60px;
      }

      /* Data info section polish */
      #data_info .value { font-weight: 700; }

      /* Shiny progress bar theming */
      .shiny-notification-close { color: var(--text-secondary) !important; }
      .progress-bar { background: var(--gradient-primary) !important; }

      /* Badge accent color utility */
      .badge-accent {
        background: var(--gradient-primary);
        color: white;
        padding: 3px 10px;
        border-radius: var(--radius-full);
        font-size: 11px;
        font-weight: 700;
        letter-spacing: 0.5px;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         INTRO.JS TOUR - force readable text on white tooltips
         ═══════════════════════════════════════════════════════════════════════════ */
      .introjs-tooltip {
        color: #1e293b !important;
        background: #ffffff !important;
        border-radius: 12px !important;
        box-shadow: 0 8px 32px rgba(0,0,0,0.25) !important;
      }
      .introjs-tooltiptext {
        color: #334155 !important;
        font-size: 14px !important;
        line-height: 1.6 !important;
      }
      .introjs-tooltip-title {
        color: #0f172a !important;
        font-weight: 700 !important;
      }
      .introjs-arrow { border-color: transparent !important; }
      .introjs-arrow.top { border-bottom-color: #ffffff !important; }
      .introjs-arrow.bottom { border-top-color: #ffffff !important; }
      .introjs-arrow.left { border-right-color: #ffffff !important; }
      .introjs-arrow.right { border-left-color: #ffffff !important; }
      .introjs-tooltipbuttons {
        border-top: 1px solid #e2e8f0 !important;
      }
      .introjs-button {
        color: #334155 !important;
        background: #f1f5f9 !important;
        border: 1px solid #cbd5e1 !important;
        border-radius: 6px !important;
        font-weight: 600 !important;
        text-shadow: none !important;
      }
      .introjs-button:hover {
        background: #e2e8f0 !important;
      }
      .introjs-button.introjs-donebutton,
      .introjs-button.introjs-nextbutton {
        color: #ffffff !important;
        background: #0f766e !important;
        border-color: #0f766e !important;
      }
      .introjs-helperLayer {
        box-shadow: 0 0 0 5000px rgba(0,0,0,0.55) !important;
      }
      .introjs-bullets ul li a {
        background: #cbd5e1 !important;
      }
      .introjs-bullets ul li a.active {
        background: #0f766e !important;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         WELCOME HERO
         ═══════════════════════════════════════════════════════════════════════════ */
      .welcome-hero {
        text-align: center;
        padding: 56px 32px 40px;
        position: relative;
        overflow: hidden;
      }
      .welcome-hero::before {
        content: "";
        position: absolute;
        top: -60%;
        left: 50%;
        transform: translateX(-50%);
        width: 800px;
        height: 800px;
        background: radial-gradient(circle, rgba(99,102,241,0.10) 0%, rgba(139,92,246,0.04) 40%, transparent 70%);
        pointer-events: none;
      }
      .welcome-icon {
        width: 80px;
        height: 80px;
        margin: 0 auto 24px;
        background: var(--gradient-primary);
        border-radius: 24px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 36px;
        color: white;
        box-shadow: 0 8px 32px rgba(99,102,241,0.35), inset 0 1px 0 rgba(255,255,255,0.2);
        animation: heroFloat 4s ease-in-out infinite;
        position: relative;
      }
      .welcome-icon::after {
        content: "";
        position: absolute;
        inset: -4px;
        border-radius: 28px;
        background: var(--gradient-primary);
        opacity: 0.15;
        filter: blur(12px);
      }
      @keyframes heroFloat {
        0%, 100% { transform: translateY(0); }
        50% { transform: translateY(-8px); }
      }
      .welcome-hero h2 {
        font-size: 28px;
        font-weight: 800;
        background: var(--gradient-primary);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        letter-spacing: -0.5px;
        margin: 0 0 8px;
        position: relative;
      }
      .welcome-hero .welcome-sub {
        font-size: 15px;
        color: var(--text-muted);
        max-width: 480px;
        margin: 0 auto 32px;
        line-height: 1.6;
      }
      .feature-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
        gap: 16px;
        max-width: 720px;
        margin: 0 auto 32px;
      }
      .feature-card {
        background: var(--bg-glass);
        border: 1px solid var(--border-subtle);
        border-radius: var(--radius-lg);
        padding: 20px 16px;
        text-align: center;
        backdrop-filter: blur(var(--blur-sm));
        -webkit-backdrop-filter: blur(var(--blur-sm));
        transition: transform 0.25s ease, border-color 0.25s ease, box-shadow 0.25s ease;
        cursor: default;
      }
      .feature-card:hover {
        transform: translateY(-4px);
        border-color: var(--border-accent);
        box-shadow: 0 12px 32px rgba(99,102,241,0.15);
      }
      .feature-card i {
        font-size: 24px;
        color: var(--accent-primary);
        margin-bottom: 10px;
        display: block;
        filter: drop-shadow(0 0 6px rgba(99,102,241,0.3));
      }
      .feature-card h4 {
        font-size: 13px;
        font-weight: 700;
        color: var(--text-primary);
        margin: 0 0 4px;
      }
      .feature-card p {
        font-size: 11px;
        color: var(--text-muted);
        margin: 0;
        line-height: 1.5;
      }
      .welcome-hint {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        background: rgba(99,102,241,0.08);
        border: 1px dashed rgba(99,102,241,0.25);
        border-radius: var(--radius-full);
        padding: 8px 20px;
        font-size: 13px;
        color: var(--accent-primary);
        font-weight: 600;
        animation: hintPulse 2.5s ease-in-out infinite;
      }
      @keyframes hintPulse {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.65; }
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         EMPTY STATE PLACEHOLDERS
         ═══════════════════════════════════════════════════════════════════════════ */
      .empty-state {
        text-align: center;
        padding: 48px 24px;
        color: var(--text-muted);
      }
      .empty-state i {
        font-size: 48px;
        opacity: 0.25;
        margin-bottom: 16px;
        display: block;
      }
      .empty-state h4 {
        font-size: 16px;
        font-weight: 700;
        color: var(--text-secondary);
        margin: 0 0 6px;
      }
      .empty-state p {
        font-size: 13px;
        max-width: 340px;
        margin: 0 auto;
        line-height: 1.6;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         ENHANCED NOTIFICATIONS
         ═══════════════════════════════════════════════════════════════════════════ */
      .shiny-notification {
        background: var(--bg-card) !important;
        border: 1px solid var(--border-subtle) !important;
        border-left: 4px solid var(--accent-primary) !important;
        border-radius: var(--radius-md) !important;
        color: var(--text-primary) !important;
        box-shadow: 0 8px 32px rgba(0,0,0,0.18), 0 2px 8px rgba(0,0,0,0.08) !important;
        font-family: "Space Grotesk", sans-serif !important;
        font-size: 13px !important;
        padding: 14px 18px !important;
        animation: notifSlide 0.3s ease;
        backdrop-filter: blur(12px);
      }
      .shiny-notification-message { border-left-color: var(--accent-info) !important; }
      .shiny-notification-warning { border-left-color: var(--accent-warning) !important; }
      .shiny-notification-error   { border-left-color: var(--accent-danger) !important; }
      .shiny-notification-close {
        color: var(--text-muted) !important;
        font-size: 18px !important;
        opacity: 0.6;
        transition: opacity 0.15s;
      }
      .shiny-notification-close:hover { opacity: 1; }
      @keyframes notifSlide {
        from { opacity: 0; transform: translateX(20px); }
        to   { opacity: 1; transform: translateX(0); }
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         IMPROVED CODE BLOCKS (verbatimTextOutput)
         ═══════════════════════════════════════════════════════════════════════════ */
      pre, .shiny-text-output {
        background: #0f172a !important;
        border: 1px solid rgba(148,163,184,0.15) !important;
        border-radius: var(--radius-md) !important;
        padding: 18px 22px !important;
        font-family: "JetBrains Mono", "Fira Code", ui-monospace, monospace !important;
        font-size: 12.5px !important;
        color: #e2e8f0 !important;
        overflow-x: auto;
        line-height: 1.75 !important;
        position: relative;
      }
      pre::before {
        content: "R";
        position: absolute;
        top: 8px;
        right: 12px;
        font-size: 10px;
        font-weight: 700;
        color: rgba(148,163,184,0.35);
        letter-spacing: 1px;
        font-family: "Space Grotesk", sans-serif;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         DARK THEME — DEEP SPACE + CYAN + GLASSMORPHISM
         ═══════════════════════════════════════════════════════════════════════════ */
      [data-theme="dark"] {
        --bg-primary: #0a0a1a;
        --bg-secondary: #12122a;
        --bg-tertiary: #1a1a3e;
        --bg-card: rgba(18, 18, 42, 0.75);
        --bg-glass: rgba(10, 10, 26, 0.6);
        --bg-glass-hover: rgba(10, 10, 26, 0.8);
        --surface-header: rgba(10, 10, 26, 0.85);
        --surface-nav: rgba(18, 18, 42, 0.7);
        --surface-control: rgba(26, 26, 62, 0.6);
        --surface-card-header: linear-gradient(180deg, rgba(0, 212, 255, 0.08) 0%, transparent 100%);
        --surface-card-footer: rgba(18, 18, 42, 0.7);
        --surface-input: rgba(26, 26, 62, 0.6);
        --surface-modal: rgba(10, 10, 26, 0.92);
        --surface-badge: rgba(18, 18, 42, 0.7);
        --surface-table-head: rgba(26, 26, 62, 0.8);
        --accent-primary: #00d4ff;
        --accent-secondary: #00b8e6;
        --accent-tertiary: #0099cc;
        --accent-success: #00ff88;
        --accent-warning: #ffd700;
        --accent-danger: #ff4757;
        --accent-info: #00d4ff;
        --text-primary: #e8e8ff;
        --text-secondary: #b8b8d0;
        --text-muted: #8888a0;
        --text-dim: #606080;
        --border-subtle: rgba(0, 212, 255, 0.12);
        --border-accent: rgba(0, 212, 255, 0.35);
        --border-glow: rgba(0, 212, 255, 0.5);
        --gradient-primary: linear-gradient(135deg, #00d4ff 0%, #00b8e6 50%, #0099cc 100%);
        --gradient-secondary: linear-gradient(135deg, #00ff88 0%, #00d4ff 100%);
        --gradient-warm: linear-gradient(135deg, #ffd700 0%, #ff4757 100%);
        --gradient-surface: linear-gradient(180deg, rgba(0, 212, 255, 0.08) 0%, transparent 100%);
        --shadow-sm: 0 2px 8px rgba(0,0,0,0.4);
        --shadow-md: 0 12px 28px rgba(0,0,0,0.5);
        --shadow-lg: 0 24px 48px rgba(0,0,0,0.6);
        --shadow-glow: 0 0 40px rgba(0, 212, 255, 0.25);
        --shadow-glass: 0 8px 32px rgba(0,0,0,0.5), inset 0 1px 0 rgba(0,212,255,0.1);
      }
      [data-theme="dark"] body {
        background:
          radial-gradient(ellipse 120% 60% at 10% -10%, rgba(0,212,255,0.1), transparent 55%),
          radial-gradient(ellipse 90% 55% at 95% 12%, rgba(0,255,136,0.06), transparent 55%),
          radial-gradient(ellipse 70% 50% at 45% 95%, rgba(0,212,255,0.05), transparent 55%),
          #0a0a1a;
      }

      /* Frosted glass header */
      [data-theme="dark"] .app-header {
        background: rgba(10, 10, 26, 0.85) !important;
        backdrop-filter: blur(24px) !important;
        -webkit-backdrop-filter: blur(24px) !important;
        border-bottom: 1px solid rgba(0, 212, 255, 0.2) !important;
        box-shadow: var(--shadow-glass), 0 0 30px rgba(0, 212, 255, 0.1) !important;
      }
      [data-theme="dark"] .app-header::after {
        background: linear-gradient(90deg, transparent 0%, rgba(0,212,255,0.3) 50%, transparent 100%) !important;
        opacity: 1 !important;
      }

      /* Frosted glass panels */
      [data-theme="dark"] .sidebar-section,
      [data-theme="dark"] .glass-card {
        background: rgba(18, 18, 42, 0.75) !important;
        border: 1px solid rgba(0, 212, 255, 0.15) !important;
        backdrop-filter: blur(16px) !important;
        -webkit-backdrop-filter: blur(16px) !important;
        box-shadow: var(--shadow-glass), 0 0 20px rgba(0, 212, 255, 0.08) !important;
      }
      [data-theme="dark"] .sidebar-section:hover,
      [data-theme="dark"] .glass-card:hover {
        border-color: rgba(0, 212, 255, 0.4) !important;
        box-shadow: var(--shadow-glass), 0 0 35px rgba(0, 212, 255, 0.15) !important;
      }

      /* Quick buttons — transparent bg, cyan borders, hover glow */
      [data-theme="dark"] .btn-icon {
        background: transparent !important;
        border: 1px solid rgba(0, 212, 255, 0.3) !important;
        color: #00d4ff !important;
      }
      [data-theme="dark"] .btn-icon:hover {
        background: rgba(0, 212, 255, 0.1) !important;
        border-color: #00d4ff !important;
        box-shadow: 0 0 15px rgba(0, 212, 255, 0.4) !important;
      }
      [data-theme="dark"] .btn-primary {
        background: linear-gradient(135deg, #00d4ff 0%, #00b8e6 100%) !important;
        border: none !important;
        color: #0a0a1a !important;
        box-shadow: 0 0 15px rgba(0, 212, 255, 0.3) !important;
      }
      [data-theme="dark"] .btn-primary:hover {
        box-shadow: 0 0 25px rgba(0, 212, 255, 0.5) !important;
      }
      [data-theme="dark"] .btn-secondary {
        background: transparent !important;
        border: 1px solid rgba(0, 212, 255, 0.25) !important;
        color: #00d4ff !important;
      }
      [data-theme="dark"] .btn-secondary:hover {
        background: rgba(0, 212, 255, 0.08) !important;
        border-color: #00d4ff !important;
        box-shadow: 0 0 15px rgba(0, 212, 255, 0.3) !important;
        color: #00d4ff !important;
      }

      /* Status badge + indicator */
      [data-theme="dark"] .status-badge {
        background: rgba(18, 18, 42, 0.9) !important;
        border: 1px solid rgba(0, 212, 255, 0.2) !important;
        color: #b8b8d0 !important;
      }
      [data-theme="dark"] .status-badge:hover {
        background: rgba(0, 212, 255, 0.08) !important;
        border-color: rgba(0, 212, 255, 0.4) !important;
        box-shadow: 0 0 15px rgba(0, 212, 255, 0.2) !important;
      }
      [data-theme="dark"] .status-indicator {
        box-shadow: 0 0 12px var(--accent-success) !important;
      }

      /* Code blocks — Monokai-inspired */
      [data-theme="dark"] pre,
      [data-theme="dark"] .shiny-text-output {
        background: #0a0a1a !important;
        border: 1px solid rgba(0, 212, 255, 0.2) !important;
        color: #a6e22e !important;
        box-shadow: 0 0 15px rgba(0, 212, 255, 0.08) !important;
      }
      [data-theme="dark"] pre::before {
        color: rgba(0, 212, 255, 0.4) !important;
      }

      /* Tables */
      [data-theme="dark"] table.dataTable {
        background: rgba(18, 18, 42, 0.8) !important;
        border-color: rgba(0, 212, 255, 0.15) !important;
      }
      [data-theme="dark"] table.dataTable thead th {
        background: linear-gradient(180deg, rgba(0,212,255,0.12) 0%, rgba(0,212,255,0.04) 100%) !important;
        color: #00d4ff !important;
        border-bottom: 2px solid rgba(0, 212, 255, 0.3) !important;
      }
      [data-theme="dark"] .table tbody tr:hover,
      [data-theme="dark"] .dataTable tbody tr:hover {
        background: rgba(0, 212, 255, 0.08) !important;
      }
      [data-theme="dark"] .dataTables_wrapper .dataTables_paginate .paginate_button {
        color: #b8b8d0 !important;
        border: 1px solid rgba(0, 212, 255, 0.15) !important;
      }
      [data-theme="dark"] .dataTables_wrapper .dataTables_paginate .paginate_button.current,
      [data-theme="dark"] .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background: rgba(0, 212, 255, 0.15) !important;
        color: #00d4ff !important;
        border-color: rgba(0, 212, 255, 0.3) !important;
      }

      /* Nav tabs — frosted glass */
      [data-theme="dark"] .nav-tabs {
        background: rgba(18, 18, 42, 0.7) !important;
        border: 1px solid rgba(0, 212, 255, 0.12) !important;
        backdrop-filter: blur(12px) !important;
        -webkit-backdrop-filter: blur(12px) !important;
      }
      [data-theme="dark"] .nav-tabs > li > a {
        color: #8888a0 !important;
      }
      [data-theme="dark"] .nav-tabs > li > a:hover {
        color: #00d4ff !important;
        background: rgba(0, 212, 255, 0.1) !important;
      }
      [data-theme="dark"] .nav-tabs > li.active > a,
      [data-theme="dark"] .nav-tabs > li.active > a:hover,
      [data-theme="dark"] .nav-tabs > li.active > a:focus {
        color: #0a0a1a !important;
        background: linear-gradient(135deg, #00d4ff 0%, #00b8e6 100%) !important;
        border-color: #00d4ff !important;
        box-shadow: 0 4px 20px rgba(0, 212, 255, 0.4) !important;
      }
      [data-theme="dark"] .nav-tabs > li.active > a::before {
        background: #00d4ff !important;
      }
      [data-theme="dark"] .nav-tabs > li.active > a i {
        color: #0a0a1a !important;
        filter: none !important;
      }

      /* Tab content area */
      [data-theme="dark"] .tab-content {
        background: rgba(18, 18, 42, 0.75) !important;
        border-color: rgba(0, 212, 255, 0.12) !important;
        backdrop-filter: blur(16px) !important;
        -webkit-backdrop-filter: blur(16px) !important;
      }

      /* Form elements — glass style */
      [data-theme="dark"] .form-control,
      [data-theme="dark"] select,
      [data-theme="dark"] input[type="text"],
      [data-theme="dark"] input[type="number"],
      [data-theme="dark"] input[type="password"],
      [data-theme="dark"] textarea {
        background: rgba(26, 26, 62, 0.6) !important;
        border: 1px solid rgba(0, 212, 255, 0.15) !important;
        color: #e8e8ff !important;
        backdrop-filter: blur(8px) !important;
        -webkit-backdrop-filter: blur(8px) !important;
      }
      [data-theme="dark"] .form-control:focus,
      [data-theme="dark"] select:focus,
      [data-theme="dark"] input:focus,
      [data-theme="dark"] textarea:focus {
        border-color: #00d4ff !important;
        box-shadow: 0 0 20px rgba(0, 212, 255, 0.2) !important;
      }

      /* Modals — frosted glass */
      [data-theme="dark"] .modal-content {
        background: rgba(10, 10, 26, 0.92) !important;
        border: 1px solid rgba(0, 212, 255, 0.2) !important;
        backdrop-filter: blur(24px) !important;
        -webkit-backdrop-filter: blur(24px) !important;
        box-shadow: 0 0 50px rgba(0, 212, 255, 0.12), 0 20px 60px rgba(0,0,0,0.6) !important;
      }
      [data-theme="dark"] .modal-header {
        background: linear-gradient(135deg, rgba(10,10,26,0.9) 0%, rgba(26,26,62,0.8) 100%) !important;
        border-bottom: 1px solid rgba(0, 212, 255, 0.2) !important;
      }

      /* Brand icon + text */
      [data-theme="dark"] .brand-icon {
        background: linear-gradient(135deg, #00d4ff 0%, #00b8e6 100%) !important;
        box-shadow: 0 0 20px rgba(0, 212, 255, 0.4) !important;
      }
      [data-theme="dark"] .brand-text h1 {
        background: linear-gradient(135deg, #00d4ff 0%, #00b8e6 100%) !important;
        -webkit-background-clip: text !important;
        -webkit-text-fill-color: transparent !important;
        background-clip: text !important;
      }
      [data-theme="dark"] .brand-text .tagline {
        color: #8888a0 !important;
      }

      /* Theme switcher dropdown */
      [data-theme="dark"] .theme-select {
        background: rgba(26, 26, 62, 0.8) !important;
        border: 1px solid rgba(0, 212, 255, 0.25) !important;
        color: #00d4ff !important;
      }
      [data-theme="dark"] .theme-select:hover {
        border-color: #00d4ff !important;
        box-shadow: 0 0 10px rgba(0, 212, 255, 0.3) !important;
      }

      /* Scrollbar */
      [data-theme="dark"] ::-webkit-scrollbar-thumb {
        background: rgba(0, 212, 255, 0.25) !important;
      }
      [data-theme="dark"] ::-webkit-scrollbar-thumb:hover {
        background: rgba(0, 212, 255, 0.45) !important;
      }
      [data-theme="dark"] ::-webkit-scrollbar-track {
        background: rgba(10, 10, 26, 0.5) !important;
      }

      /* Chips */
      [data-theme="dark"] .chip {
        background: rgba(0, 212, 255, 0.1) !important;
        color: #00d4ff !important;
        border: 1px solid rgba(0, 212, 255, 0.25) !important;
      }

      /* Stat cards */
      [data-theme="dark"] .stat-card {
        background: rgba(18, 18, 42, 0.95) !important;
        border: 1px solid rgba(0, 212, 255, 0.12) !important;
        box-shadow: 0 0 15px rgba(0, 212, 255, 0.08) !important;
      }
      [data-theme="dark"] .stat-card:hover {
        border-color: rgba(0, 212, 255, 0.3) !important;
        box-shadow: 0 0 20px rgba(0, 212, 255, 0.15) !important;
      }
      [data-theme="dark"] .stat-card .value {
        background: linear-gradient(135deg, #00d4ff 0%, #00b8e6 100%) !important;
        -webkit-background-clip: text !important;
        -webkit-text-fill-color: transparent !important;
        background-clip: text !important;
      }
      [data-theme="dark"] .stat-card::before {
        background: linear-gradient(135deg, #00d4ff 0%, #00b8e6 100%) !important;
      }

      /* Chart containers */
      [data-theme="dark"] .chart-container {
        background: rgba(18, 18, 42, 0.95) !important;
        border: 1px solid rgba(0, 212, 255, 0.12) !important;
        box-shadow: 0 0 15px rgba(0, 212, 255, 0.08) !important;
      }
      [data-theme="dark"] .chart-container:hover {
        border-color: rgba(0, 212, 255, 0.3) !important;
        box-shadow: 0 0 20px rgba(0, 212, 255, 0.15) !important;
      }

      /* Welcome hero */
      [data-theme="dark"] .welcome-hero::before {
        background: radial-gradient(circle, rgba(0,212,255,0.08) 0%, rgba(0,212,255,0.03) 40%, transparent 70%) !important;
      }
      [data-theme="dark"] .welcome-icon {
        background: linear-gradient(135deg, #00d4ff 0%, #00b8e6 100%) !important;
        box-shadow: 0 8px 32px rgba(0,212,255,0.35) !important;
      }
      [data-theme="dark"] .welcome-hero h2 {
        background: linear-gradient(135deg, #00d4ff 0%, #00b8e6 100%) !important;
        -webkit-background-clip: text !important;
        -webkit-text-fill-color: transparent !important;
        background-clip: text !important;
      }
      [data-theme="dark"] .feature-card {
        background: rgba(18, 18, 42, 0.8) !important;
        border: 1px solid rgba(0, 212, 255, 0.15) !important;
      }
      [data-theme="dark"] .feature-card:hover {
        border-color: rgba(0, 212, 255, 0.35) !important;
        box-shadow: 0 0 20px rgba(0, 212, 255, 0.15) !important;
      }
      [data-theme="dark"] .feature-card i {
        color: #00d4ff !important;
        filter: drop-shadow(0 0 8px rgba(0,212,255,0.4)) !important;
      }
      [data-theme="dark"] .welcome-hint {
        background: rgba(0, 212, 255, 0.08) !important;
        border: 1px dashed rgba(0, 212, 255, 0.3) !important;
        color: #00d4ff !important;
      }

      /* Section titles */
      [data-theme="dark"] .section-title {
        color: #00d4ff !important;
        border-bottom-color: rgba(0, 212, 255, 0.15) !important;
      }

      /* Notifications */
      [data-theme="dark"] .shiny-notification {
        background: rgba(18, 18, 42, 0.98) !important;
        border: 1px solid rgba(0, 212, 255, 0.2) !important;
        box-shadow: 0 0 20px rgba(0, 212, 255, 0.15), 0 8px 32px rgba(0,0,0,0.4) !important;
      }

      /* Slider */
      [data-theme="dark"] .irs--shiny .irs-bar {
        background: linear-gradient(135deg, #00d4ff 0%, #00b8e6 100%) !important;
      }
      [data-theme="dark"] .irs--shiny .irs-handle {
        border-color: #00d4ff !important;
        background: #12122a !important;
        box-shadow: 0 0 10px rgba(0, 212, 255, 0.3) !important;
      }
      [data-theme="dark"] .irs--shiny .irs-line {
        background: rgba(26, 26, 62, 0.8) !important;
        border-color: rgba(0, 212, 255, 0.15) !important;
      }

      /* Card header */
      [data-theme="dark"] .card-header {
        background: linear-gradient(180deg, rgba(0,212,255,0.06) 0%, transparent 100%) !important;
        border-bottom-color: rgba(0, 212, 255, 0.12) !important;
      }
      [data-theme="dark"] .card-title i {
        color: #00d4ff !important;
      }

      /* Focus outlines */
      [data-theme="dark"] .btn-icon:focus,
      [data-theme="dark"] .btn-primary:focus,
      [data-theme="dark"] .btn-secondary:focus,
      [data-theme="dark"] select:focus,
      [data-theme="dark"] input:focus,
      [data-theme="dark"] textarea:focus {
        outline: 3px solid rgba(0, 212, 255, 0.15) !important;
      }

      /* Label text */
      [data-theme="dark"] label.control-label {
        color: #b8b8d0 !important;
      }

      /* File input button */
      [data-theme="dark"] .input-group-btn .btn {
        background: rgba(26, 26, 62, 0.8) !important;
        border-color: rgba(0, 212, 255, 0.2) !important;
        color: #b8b8d0 !important;
      }
      [data-theme="dark"] .input-group-btn .btn:hover {
        background: rgba(0, 212, 255, 0.1) !important;
        border-color: #00d4ff !important;
        color: #00d4ff !important;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         MICRO-INTERACTIONS & POLISH
         ═══════════════════════════════════════════════════════════════════════════ */
      @keyframes fadeInUp {
        from { opacity: 0; transform: translateY(16px); }
        to { opacity: 1; transform: translateY(0); }
      }
      .tab-pane { animation: fadeInUp 0.35s ease; }

      .sidebar-section {
        animation: fadeInUp 0.4s ease backwards;
      }
      .sidebar-section:nth-child(1) { animation-delay: 0.05s; }
      .sidebar-section:nth-child(2) { animation-delay: 0.1s; }
      .sidebar-section:nth-child(3) { animation-delay: 0.15s; }

      .stat-card {
        animation: fadeInUp 0.35s ease backwards;
      }
      .stat-cards .stat-card:nth-child(1) { animation-delay: 0.05s; }
      .stat-cards .stat-card:nth-child(2) { animation-delay: 0.1s; }
      .stat-cards .stat-card:nth-child(3) { animation-delay: 0.15s; }
      .stat-cards .stat-card:nth-child(4) { animation-delay: 0.2s; }
      .stat-cards .stat-card:nth-child(5) { animation-delay: 0.25s; }

      .chart-container {
        animation: fadeInUp 0.4s ease backwards;
        animation-delay: 0.1s;
      }

      /* Ripple on primary buttons */
      .btn-primary { position: relative; overflow: hidden; }
      .btn-primary::after {
        content: "";
        position: absolute;
        inset: 0;
        background: radial-gradient(circle at var(--x, 50%) var(--y, 50%), rgba(255,255,255,0.25) 0%, transparent 60%);
        opacity: 0;
        transition: opacity 0.4s;
        pointer-events: none;
      }
      .btn-primary:active::after { opacity: 1; }

      /* Glow ring on active tab */
      .nav-tabs > li.active > a {
        box-shadow: 0 -2px 12px rgba(15,118,110,0.08), 0 0 0 1px rgba(15,118,110,0.08) !important;
      }

      /* Subtle shine on stat cards */
      .stat-card::before {
        content: "";
        position: absolute;
        top: 0; left: 0; right: 0;
        height: 3px;
        background: var(--gradient-primary);
        opacity: 0;
        transition: opacity 0.25s;
      }
      .stat-card:hover::before { opacity: 1; }

      /* Better scrollbar inside sidebar sections */
      .sidebar-section::-webkit-scrollbar { width: 4px; }
      .sidebar-section::-webkit-scrollbar-thumb {
        background: rgba(15,118,110,0.2);
        border-radius: 4px;
      }

      /* Smooth tooltip-like label on chart container headers */
      .chart-header h5 { transition: color 0.15s; }
      .chart-container:hover .chart-header h5 { color: var(--accent-primary); }

      /* Sidebar launchpad buttons polish */
      .sidebar-section .btn-secondary.btn-block {
        text-align: left;
        border-left: 3px solid transparent;
        transition: border-color 0.2s, background 0.2s, transform 0.15s, box-shadow 0.2s;
      }
      .sidebar-section .btn-secondary.btn-block:hover {
        border-left-color: var(--accent-primary) !important;
        transform: translateX(2px);
        box-shadow: 0 2px 8px rgba(15,118,110,0.1);
      }
      .sidebar-section .btn-secondary.btn-block:hover i {
        color: var(--accent-primary);
        transform: scale(1.15);
        transition: transform 0.2s, color 0.15s;
      }

      /* Sidebar data info stat cards */
      .sidebar .stat-card {
        padding: 14px 12px;
        margin-bottom: 8px;
        border-radius: var(--radius-md);
        background: var(--bg-glass);
        border: 1px solid var(--border-subtle);
      }
      .sidebar .stat-card .value { font-size: 22px; }
      .sidebar .stat-card .label { font-size: 10px; margin-top: 4px; }

      /* Chat timestamp styling */
      .chat-timestamp {
        font-size: 10px;
        color: var(--text-dim);
        text-align: center;
        margin: 12px 0;
        letter-spacing: 0.5px;
      }

      /* Better chat send button */
      .chat-main .btn-primary {
        border-radius: var(--radius-lg) !important;
        font-size: 18px;
      }

      /* Improve selectInput option group labels */
      select optgroup {
        font-weight: 700;
        color: var(--text-muted);
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.8px;
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         RESPONSIVE POLISH
         ═══════════════════════════════════════════════════════════════════════════ */
      @media (max-width: 992px) {
        .feature-grid { grid-template-columns: repeat(2, 1fr); }
        .welcome-hero { padding: 36px 20px 28px; }
        .welcome-hero h2 { font-size: 22px; }
        .stat-cards { grid-template-columns: repeat(2, 1fr); }
      }
      @media (max-width: 768px) {
        .app-header { padding: 10px 16px; }
        .brand-text h1 { font-size: 18px; }
        .brand-text .tagline { display: none; }
        .header-controls .status-badge span { display: none; }
        .feature-grid { grid-template-columns: 1fr; max-width: 320px; }
        .tab-content { padding: 16px; }
        .nav-tabs > li > a { padding: 10px 12px !important; font-size: 12px !important; }
      }

      /* ═══════════════════════════════════════════════════════════════════════════
         AI INSIGHTS CARD
         ═══════════════════════════════════════════════════════════════════════════ */
      .ai-insights-card {
        padding: 18px 20px;
        background: var(--bg-card);
        border: 1px solid var(--border-subtle);
        border-radius: var(--radius-lg);
        margin: 12px 0;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04);
      }
      .ai-insights-card.ai-insights-warning {
        background: #fffbeb;
        border-color: #f59e0b;
      }
      [data-theme="dark"] .ai-insights-card.ai-insights-warning {
        background: rgba(245,158,11,0.08);
        border-color: rgba(245,158,11,0.35);
      }
      .ai-insights-header {
        font-size: 14px;
        font-weight: 700;
        color: var(--text-primary);
        margin-bottom: 10px;
        letter-spacing: 0.1px;
      }
      .ai-insights-warning .ai-insights-header {
        color: #92400e;
      }
      [data-theme="dark"] .ai-insights-warning .ai-insights-header {
        color: #fbbf24;
      }
      .ai-insights-muted {
        font-size: 13px;
        color: var(--text-secondary);
        margin-bottom: 8px;
        line-height: 1.5;
      }
      .ai-insights-warning .ai-insights-muted {
        color: #92400e;
      }
      [data-theme="dark"] .ai-insights-warning .ai-insights-muted {
        color: #fbbf24;
      }
      .ai-insights-list {
        list-style: none;
        padding: 0;
        margin: 0;
      }
      .ai-insight-item {
        display: flex;
        align-items: flex-start;
        gap: 10px;
        padding: 8px 0;
        font-size: 13px;
        line-height: 1.6;
        color: var(--text-primary);
        border-bottom: 1px solid rgba(0,0,0,0.04);
      }
      .ai-insight-item:last-child { border-bottom: none; }
      .ai-insights-warning .ai-insight-item {
        color: #78350f;
        border-bottom-color: rgba(245,158,11,0.12);
      }
      [data-theme="dark"] .ai-insights-warning .ai-insight-item {
        color: #fde68a;
        border-bottom-color: rgba(245,158,11,0.15);
      }
      .ai-insight-bullet {
        flex-shrink: 0;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 24px;
        height: 24px;
        border-radius: var(--radius-full);
        background: rgba(15,118,110,0.1);
        color: var(--accent-primary);
        font-size: 10px;
        font-weight: 700;
        margin-top: 1px;
      }
      .ai-insights-warning .ai-insight-bullet {
        background: rgba(245,158,11,0.15);
        color: #92400e;
      }
      [data-theme="dark"] .ai-insights-warning .ai-insight-bullet {
        background: rgba(245,158,11,0.18);
        color: #fbbf24;
      }
    '))
  ),

  # ═══════════════════════════════════════════════════════════════════════════════
  # HEADER
  # ═══════════════════════════════════════════════════════════════════════════════
  div(class = "app-header",
    div(class = "header-brand",
      div(class = "brand-icon", tags$i(class = "fas fa-chart-line")),
      div(class = "brand-text",
        tags$h1("DataExplorerPro"),
        div(class = "tagline", "EDA, dashboards, and AI insights for teams")
      )
    ),
    div(class = "header-controls",
      div(id = "ollama_status_badge", class = "status-badge",
        onclick = "Shiny.setInputValue('ollama_status_click', Math.random())",
        div(class = "status-indicator"),
        span("Ollama Ready")
      ),
      div(class = "theme-switcher",
        tags$select(id = "theme_select", class = "theme-select",
          onchange = "window.depSetTheme(this.value, {pushToShiny: true});",
          tags$option(value = "light", "Default (Light)"),
          tags$option(value = "dark", "Dark"),
          tags$option(value = "cobalt", "Cobalt"),
          tags$option(value = "solarize", "Solarize"),
          tags$option(value = "blue", "Blue"),
          tags$option(value = "green", "Green"),
          tags$option(value = "purple", "Purple"),
          tags$option(value = "orange", "Orange"),
          tags$option(value = "red", "Red"),
          tags$option(value = "monochrome", "Mono")
        )
      ),
      actionButton("toggle_sidebar", "", class = "btn-icon", icon = icon("compress-alt")),
      actionButton("show_help", "", class = "btn-icon", icon = icon("question-circle")),
      actionButton("show_tour", "", class = "btn-icon", icon = icon("info-circle")),
      actionButton("show_shortcuts", "", class = "btn-icon", icon = icon("keyboard")),
      actionButton("settings_btn", "", class = "btn-icon", icon = icon("cog"))
    )
  ),

  # ═══════════════════════════════════════════════════════════════════════════════
  # MAIN LAYOUT
  # ═══════════════════════════════════════════════════════════════════════════════
  div(class = "main-container",
    # SIDEBAR
    div(class = "sidebar",
      # Data Source Section
      div(class = "sidebar-section",
        div(class = "section-title", tags$i(class = "fas fa-database"), "Data Workspace"),
        fileInput("data_file", "Upload Dataset",
                  accept = c(".csv", ".tsv", ".xlsx", ".rds", ".parquet"),
                  placeholder = "Choose file..."),
        selectInput("sample_data", "Or load sample data:",
                    choices = c("None" = "none", 
                                "mtcars" = "mtcars",
                                "iris" = "iris",
                                "diamonds" = "diamonds",
                                "penguins" = "penguins",
                                "gapminder" = "gapminder",
                                "airquality (NYC Air, 1973)" = "airquality",
                                "USArrests (US Crime)" = "usarrests",
                                "CO2 (Mauna Loa)" = "co2",
                                "ChickWeight (Growth)" = "chickweight",
                                "ToothGrowth (Vitamin C)" = "toothgrowth"),
                    selected = "none"),
        actionButton("load_data", "Load Dataset", icon = icon("upload"), 
                    class = "btn-primary btn-block"),
        hr(),
        uiOutput("data_info")
      ),
      
      # Variables Section
      div(class = "sidebar-section",
        div(class = "section-title", tags$i(class = "fas fa-list"), "Variables"),
        uiOutput("variable_list")
      ),
      
      # Quick Actions Section
      div(class = "sidebar-section",
        div(class = "section-title", tags$i(class = "fas fa-bolt"), "Launchpad"),
        actionButton("auto_eda", "Generate Report", icon = icon("magic"), class = "btn-secondary btn-block"),
        actionButton("generate_summary", "Summary Profile", icon = icon("file-alt"), class = "btn-secondary btn-block mt-2"),
        actionButton("show_correlations", "Correlations", icon = icon("project-diagram"), class = "btn-secondary btn-block mt-2"),
        actionButton("detect_outliers", "Outliers", icon = icon("exclamation-triangle"), class = "btn-secondary btn-block mt-2")
      )
    ),
    
    # MAIN CONTENT
    div(class = "glass-card",
      tabsetPanel(id = "main_tabs", type = "tabs",
        # ═══════════════════════════════════════════════════════════════════
        # TAB 1: VISUALIZATION STUDIO
        # ═══════════════════════════════════════════════════════════════════
        tabPanel(
          title = tagList(icon("palette"), "Studio"),
          value = "viz",
          # Welcome hero — visible when no data is loaded
          conditionalPanel(
            condition = "!output.has_data",
            div(class = "welcome-hero",
              div(class = "welcome-icon", tags$i(class = "fas fa-chart-line")),
              tags$h2("Welcome to DataExplorerPro"),
              div(class = "welcome-sub", "Upload a dataset or pick a sample to unlock interactive visualizations, automated EDA reports, and AI-powered insights."),
              div(class = "feature-grid",
                div(class = "feature-card",
                  tags$i(class = "fas fa-palette"),
                  tags$h4("20+ Chart Types"),
                  tags$p("Scatter, violin, sunburst, radar, and more")
                ),
                div(class = "feature-card",
                  tags$i(class = "fas fa-robot"),
                  tags$h4("AI Insights"),
                  tags$p("Ollama-powered analysis of your data")
                ),
                div(class = "feature-card",
                  tags$i(class = "fas fa-file-alt"),
                  tags$h4("Auto EDA"),
                  tags$p("One-click exploratory reports")
                ),
                div(class = "feature-card",
                  tags$i(class = "fas fa-download"),
                  tags$h4("Pub-Ready Export"),
                  tags$p("PNG, SVG, PDF at 300 DPI")
                )
              ),
              div(class = "welcome-hint",
                tags$i(class = "fas fa-arrow-left"),
                "Upload or select a dataset in the sidebar to begin"
              )
            )
          ),
          # Studio controls + chart — visible when data IS loaded
          conditionalPanel(
            condition = "output.has_data",
            fluidRow(
              column(3,
                div(class = "sidebar-section", style = "margin-top: 0; height: calc(100vh - 200px); overflow-y: auto; padding: 18px;",
                  div(class = "section-title", tags$i(class = "fas fa-chart-bar"), "Chart Type"),
                  selectInput("chart_type", NULL,
                    choices = c(
                      "Basic Charts" = "",
                      "Scatter Plot" = "scatter",
                      "Line Chart" = "line",
                      "Bar Chart" = "bar",
                      "Histogram" = "histogram",
                      "Box Plot" = "box",
                      "Violin Plot" = "violin",
                      "Heatmap" = "heatmap",
                      "Density Plot" = "density",
                      "Pie Chart" = "pie",
                      "Advanced Charts" = "",
                      "Violin-Box Combo" = "violin_box",
                      "Beeswarm Plot" = "beeswarm",
                      "Dumbbell Plot" = "dumbbell",
                      "Statistical" = "",
                      "Forest Plot" = "forest",
                      "Bland-Altman" = "bland_altman",
                      "Time Series" = "",
                      "Stacked Area" = "stacked_area",
                      "Multivariate" = "",
                      "Bubble Chart" = "bubble",
                      "Sunburst Chart" = "sunburst",
                      "Treemap" = "treemap",
                      "Parallel Coords" = "parallel",
                      "Radar Chart" = "radar"
                    ),
                    selected = "scatter"
                  ),
                  uiOutput("chart_availability"),
                  
                  hr(),
                  div(class = "section-title", tags$i(class = "fas fa-sliders-h"), "Variables"),
                  uiOutput("x_var_selector"),
                  uiOutput("y_var_selector"),
                  uiOutput("color_var_selector"),
                  uiOutput("size_var_selector"),
                  uiOutput("group_var_selector"),
                  uiOutput("facet_var_selector"),
                  
                  hr(),
                  div(class = "section-title", tags$i(class = "fas fa-cog"), "Options"),
                  checkboxInput("auto_create_chart", "Auto-create chart on variable changes", value = TRUE),
                  checkboxInput("show_trendline", "Add Trendline", value = FALSE),
                  
                  hr(),
                  div(class = "section-title", tags$i(class = "fas fa-paint-brush"), "Style"),
                  selectInput("chart_theme", "Publication Theme:",
                    choices = c("Default" = "default", "APA" = "apa", "Nature" = "nature", "Science" = "science", "PLOS" = "plos"),
                    selected = "default"
                  ),
                  selectInput("color_palette", "Color Palette:",
                    choices = c("Viridis" = "viridis", "Magma" = "magma", "Plasma" = "plasma", "Inferno" = "inferno", "Set2" = "set2"),
                    selected = "viridis"
                  ),
                  numericInput("point_size", "Point Size:", value = 8, min = 1, max = 30, step = 1),
                  numericInput("opacity", "Opacity:", value = 0.7, min = 0.1, max = 1, step = 0.1),
                  
                  hr(),
                  actionButton("create_chart", "Create Chart", icon = icon("chart-line"), class = "btn-primary btn-success btn-block"),
                  
                  hr(),
                  div(class = "section-title", tags$i(class = "fas fa-download"), "Export"),
                  selectInput("chart_export_format", "Format:",
                    choices = c("HTML" = "html", "PNG (300 DPI)" = "png", "SVG" = "svg", "PDF" = "pdf"),
                    selected = "html"
                  ),
                  downloadButton("download_chart", "Export Chart", class = "btn-secondary btn-block")
                )
              ),
              column(9,
                div(class = "chart-container chart-main",
                  div(style = "display: flex; align-items: center; justify-content: space-between;",
                    h5(tags$i(class = "fas fa-chart-area"), "Live Preview"),
                    actionButton("expand_chart", "Expand", icon = icon("expand"), class = "btn-secondary btn-sm")
                  ),
                  div(class = "chart-plot-area",
                    plotlyOutput("main_chart", height = "100%", width = "100%")
                  )
                ),
                div(class = "chart-container",
                  h5(tags$i(class = "fas fa-code"), "Reproducible Code"),
                  verbatimTextOutput("chart_code")
                ),
                div(class = "chart-container",
                  h5(tags$i(class = "fas fa-lightbulb"), "AI Notes"),
                  uiOutput("chart_insights")
                )
              )
            )
          )
        ),
        
        # ═══════════════════════════════════════════════════════════════════
        # TAB 2: NATURAL LANGUAGE QUERY
        # ═══════════════════════════════════════════════════════════════════
        tabPanel(
          title = tagList(icon("comments"), "Ask"),
          value = "nlp",
          fluidRow(class = "chat-layout",
            column(4,
              div(class = "sidebar-section", style = "margin-top: 0;",
                div(class = "section-title", tags$i(class = "fas fa-comment-dots"), "Ask a Question"),
                selectInput("sample_nlp_query", "Sample Questions:",
                  choices = c(
                    "Load a dataset to get context-aware sample questions..." = ""
                  ),
                  selected = ""
                ),
                textAreaInput("nlp_query", NULL,
                  placeholder = "e.g., 'Show me the top 5 customers by sales' or 'Create a distribution plot of age by gender'",
                  rows = 5
                ),
                selectInput("query_type", "Query Type:",
                  choices = c("Data Transformation" = "transform", "Visualization" = "visualize", "Statistical Analysis" = "stats", "General Question" = "general"),
                  selected = "transform"
                ),
                actionButton("submit_query", "Submit Query", icon = icon("paper-plane"), class = "btn-primary btn-block"),
                hr(),
                div(class = "section-title", tags$i(class = "fas fa-history"), "History"),
                uiOutput("query_history")
              )
            ),
            column(8,
              div(class = "query-box",
                h5(tags$i(class = "fas fa-robot"), " AI Response"),
                uiOutput("query_response")
              ),
              div(class = "response-box",
                h5(tags$i(class = "fas fa-code"), " Suggested Code"),
                verbatimTextOutput("generated_code"),
                div(style = "margin-top: 12px; display: flex; gap: 8px;",
                  actionButton("copy_code", "", icon = icon("copy"), class = "btn-secondary btn-sm"),
                  actionButton("run_code", "Run Code", icon = icon("play"), class = "btn-primary btn-sm")
                )
              ),
              div(class = "chart-container",
                h5(tags$i(class = "fas fa-table"), "Query Result"),
                uiOutput("query_result")
              )
            )
          )
        ),
        
        # ═══════════════════════════════════════════════════════════════════
        # TAB 3: AUTO EDA
        # ═══════════════════════════════════════════════════════════════════
        tabPanel(
          title = tagList(icon("file-alt"), "Report"),
          value = "eda",
          fluidRow(
            # EDA Options Sidebar
            column(3,
              div(class = "sidebar-section eda-options-panel", style = "margin-top: 0; max-height: calc(100vh - 200px); overflow-y: auto; padding-right: 10px;",
                div(class = "section-title", tags$i(class = "fas fa-cogs"), "EDA Options"),
                h6("Select Charts to Include:"),
                checkboxGroupInput("eda_chart_types", NULL,
                  choices = c(
                    "Data Preview Table" = "data_preview",
                    "Histograms" = "histograms",
                    "Density Plots" = "density",
                    "Box Plots" = "box_plots",
                    "Violin Plots" = "violin_plots",
                    "Scatter Matrix" = "scatter_matrix",
                    "Correlation Heatmap" = "correlation",
                    "Pie Charts" = "pie_charts",
                    "Bar Charts" = "bar_charts",
                    "Q-Q Plots" = "qq_plots",
                    "Missing Values" = "missing_values",
                    "Summary Statistics" = "summary_stats",
                    "Data Quality Report" = "quality_report",
                    "AI Insights" = "ai_insights"
                  ),
                  selected = c("data_preview", "histograms", "box_plots", "correlation", "pie_charts", "summary_stats", "quality_report")
                ),
                hr(),
                checkboxInput("show_sample_preview", "Show sampled data preview", value = TRUE),
                actionButton("run_eda_full", "Run EDA on Full Data (one-off)", class = "btn-warning btn-block mt-2"),
                uiOutput("sample_preview_info"),
                hr(),
                actionButton("refresh_eda", "Refresh EDA", icon = icon("sync"), class = "btn-primary btn-block"),
                downloadButton("download_eda_report", "Download Report", class = "btn-secondary btn-block mt-2")
              )
            ),
            # EDA Content Area
            column(9,
              div(class = "stat-cards",
                div(class = "stat-card", uiOutput("stat_rows")),
                div(class = "stat-card", uiOutput("stat_cols")),
                div(class = "stat-card", uiOutput("stat_missing")),
                div(class = "stat-card", uiOutput("stat_numeric")),
                div(class = "stat-card", uiOutput("stat_categorical"))
              ),
              # Data Preview
              conditionalPanel(
                condition = "input.eda_chart_types.includes('data_preview')",
                div(class = "chart-container",
                  div(class = "chart-header",
                    h5(class = "chart-title", tags$i(class = "fas fa-table"), "Data Preview"),
                    actionButton("expand_eda_preview", "", icon = icon("expand"), class = "btn-icon")
                  ),
                  DTOutput("data_preview", height = "300px")
                )
              ),
              
              # Histograms
              conditionalPanel(
                condition = "input.eda_chart_types.includes('histograms')",
                div(class = "chart-container",
                  div(class = "chart-header",
                    h5(class = "chart-title", tags$i(class = "fas fa-chart-bar"), "Distribution Analysis (Histograms)"),
                    actionButton("expand_eda_histograms", "", icon = icon("expand"), class = "btn-icon")
                  ),
                  plotlyOutput("dist_plots", height = "500px")
                )
              ),
              
              # Density Plots
              conditionalPanel(
                condition = "input.eda_chart_types.includes('density')",
                div(class = "chart-container",
                  div(class = "chart-header",
                    h5(class = "chart-title", tags$i(class = "fas fa-wave-square"), "Density Plots"),
                    actionButton("expand_eda_density", "", icon = icon("expand"), class = "btn-icon")
                  ),
                  plotlyOutput("eda_density_plots", height = "500px")
                )
              ),
              
              # Box Plots
              conditionalPanel(
                condition = "input.eda_chart_types.includes('box_plots')",
                div(class = "chart-container",
                  div(class = "chart-header",
                    h5(class = "chart-title", tags$i(class = "fas fa-chart-area"), "Box Plots by Category"),
                    actionButton("expand_eda_boxplots", "", icon = icon("expand"), class = "btn-icon")
                  ),
                  plotlyOutput("box_plots", height = "500px")
                )
              ),
              
              # Violin Plots
              conditionalPanel(
                condition = "input.eda_chart_types.includes('violin_plots')",
                div(class = "chart-container",
                  div(class = "chart-header",
                    h5(class = "chart-title", tags$i(class = "fas fa-guitar"), "Violin Plots"),
                    actionButton("expand_eda_violin", "", icon = icon("expand"), class = "btn-icon")
                  ),
                  plotlyOutput("eda_violin_plots", height = "500px")
                )
              ),
              
              # Scatter Matrix
              conditionalPanel(
                condition = "input.eda_chart_types.includes('scatter_matrix')",
                div(class = "chart-container",
                  div(class = "chart-header",
                    h5(class = "chart-title", tags$i(class = "fas fa-th"), "Scatter Matrix"),
                    actionButton("expand_eda_scatter", "", icon = icon("expand"), class = "btn-icon")
                  ),
                  plotlyOutput("eda_scatter_matrix", height = "600px")
                )
              ),
              
              # Correlation Heatmap
              conditionalPanel(
                condition = "input.eda_chart_types.includes('correlation')",
                div(class = "chart-container",
                  div(class = "chart-header",
                    h5(class = "chart-title", tags$i(class = "fas fa-project-diagram"), "Correlation Matrix"),
                    actionButton("expand_eda_correlation", "", icon = icon("expand"), class = "btn-icon")
                  ),
                  plotlyOutput("correlation_plot", height = "450px")
                )
              ),
              
              # Pie Charts
              conditionalPanel(
                condition = "input.eda_chart_types.includes('pie_charts')",
                div(class = "chart-container",
                  div(class = "chart-header",
                    h5(class = "chart-title", tags$i(class = "fas fa-chart-pie"), "Categorical Variable Analysis (Pie)"),
                    actionButton("expand_eda_pie", "", icon = icon("expand"), class = "btn-icon")
                  ),
                  plotlyOutput("categorical_plots", height = "500px")
                )
              ),
              
              # Bar Charts
              conditionalPanel(
                condition = "input.eda_chart_types.includes('bar_charts')",
                div(class = "chart-container",
                  div(class = "chart-header",
                    h5(class = "chart-title", tags$i(class = "fas fa-chart-bar"), "Categorical Bar Charts"),
                    actionButton("expand_eda_bar", "", icon = icon("expand"), class = "btn-icon")
                  ),
                  plotlyOutput("eda_bar_charts", height = "500px")
                )
              ),
              
              # Q-Q Plots
              conditionalPanel(
                condition = "input.eda_chart_types.includes('qq_plots')",
                div(class = "chart-container",
                  div(class = "chart-header",
                    h5(class = "chart-title", tags$i(class = "fas fa-chart-line"), "Q-Q Plots (Normality Check)"),
                    actionButton("expand_eda_qq", "", icon = icon("expand"), class = "btn-icon")
                  ),
                  plotlyOutput("eda_qq_plots", height = "500px")
                )
              ),
              
              # Missing Values
              conditionalPanel(
                condition = "input.eda_chart_types.includes('missing_values')",
                div(class = "chart-container",
                  div(class = "chart-header",
                    h5(class = "chart-title", tags$i(class = "fas fa-exclamation-triangle"), "Missing Values Analysis"),
                    actionButton("expand_eda_missing", "", icon = icon("expand"), class = "btn-icon")
                  ),
                  plotlyOutput("eda_missing_plot", height = "400px")
                )
              ),
              
              # Summary Statistics
              conditionalPanel(
                condition = "input.eda_chart_types.includes('summary_stats')",
                div(class = "chart-container",
                  div(class = "chart-header",
                    h5(class = "chart-title", tags$i(class = "fas fa-table"), "Summary Statistics Table"),
                    actionButton("expand_eda_summary", "", icon = icon("expand"), class = "btn-icon")
                  ),
                  DTOutput("summary_stats_table", height = "400px")
                )
              ),
              
              # Data Quality Report
              conditionalPanel(
                condition = "input.eda_chart_types.includes('quality_report')",
                div(class = "chart-container",
                  h5(tags$i(class = "fas fa-info-circle"), "Data Quality Report"),
                  uiOutput("eda_summary")
                )
              ),
              
              # AI Insights
              conditionalPanel(
                condition = "input.eda_chart_types.includes('ai_insights')",
                div(class = "chart-container",
                  h5(tags$i(class = "fas fa-lightbulb"), "AI-Generated Insights"),
                  uiOutput("ai_insights")
                )
              )
            )
          )
        ),
        
        # ═══════════════════════════════════════════════════════════════════
        # TAB 4: AI CHAT
        # ═══════════════════════════════════════════════════════════════════
        tabPanel(
          title = tagList(icon("robot"), "AI Chat"),
          value = "chat",
          fluidRow(
            column(4,
              div(class = "sidebar-section", style = "margin-top: 0;",
                div(class = "section-title", tags$i(class = "fas fa-cog"), "Model Settings"),
                selectInput("ollama_model", "Ollama Model:",
                  choices = NULL,
                  selected = NULL
                ),
                numericInput("temperature", "Temperature:", value = 0.7, min = 0, max = 1, step = 0.1),
                numericInput("max_tokens", "Max Tokens:", value = 2000, min = 100, max = 10000),
                actionButton("clear_chat", "Clear Chat", icon = icon("trash"), class = "btn-secondary btn-block")
              ),
              div(class = "sidebar-section",
                div(class = "section-title", tags$i(class = "fas fa-info-circle"), "Context"),
                uiOutput("chat_context")
              )
            ),
            column(8, class = "chat-main",
              div(class = "chat-container", uiOutput("chat_messages")),
              fluidRow(
                column(10,
                  textAreaInput("chat_input", NULL, placeholder = "Ask me anything about your data...", rows = 2)
                ),
                column(2,
                  actionButton("send_message", "", icon = icon("paper-plane"), class = "btn-primary btn-block", style = "height: 60px; margin-top: 0;")
                )
              )
            )
          )
        ),
        
        # ═══════════════════════════════════════════════════════════════════
        # TAB 5: DATA TRANSFORM
        # ═══════════════════════════════════════════════════════════════════
        tabPanel(
          title = tagList(icon("filter"), "Transform"),
          value = "transform",
          fluidRow(
            column(4,
              div(class = "sidebar-section", style = "margin-top: 0;",
                div(class = "section-title", tags$i(class = "fas fa-tools"), "Transformation"),
                selectInput("transform_type", "Type:",
                  choices = c("Filter Rows" = "filter", "Select Columns" = "select", "Rename Columns" = "rename", "Handle Missing" = "missing", "Create Variable" = "new_var", "Group & Summarize" = "group_summarize", "Sort Data" = "sort"),
                  selected = "filter"
                ),
                uiOutput("transform_ui"),
                actionButton("apply_transform", "Apply", icon = icon("check"), class = "btn-primary btn-success btn-block"),
                hr(),
                actionButton("reset_data", "Reset to Original", icon = icon("undo"), class = "btn-secondary btn-block")
              )
            ),
            column(8,
              div(class = "chart-container",
                h5(tags$i(class = "fas fa-code"), "Transformation Code"),
                pre(id = "transform_code", class = "code-block", "No transformation applied yet..."),
                actionButton("copy_transform", "", icon = icon("copy"), class = "btn-secondary btn-sm", style = "margin-top: 12px;")
              ),
              div(class = "chart-container",
                h5(tags$i(class = "fas fa-table"), "Transformed Data"),
                DTOutput("transformed_preview", height = "350px")
              )
            )
          )
        ),
        
        # ═══════════════════════════════════════════════════════════════════
        # TAB 6: EXPORT & REPORTS
        # ═══════════════════════════════════════════════════════════════════
        tabPanel(
          title = tagList(icon("download"), "Export"),
          value = "export",
          fluidRow(
            column(4,
              div(class = "sidebar-section", style = "margin-top: 0;",
                div(class = "section-title", tags$i(class = "fas fa-file-export"), "Export Data"),
                radioButtons("export_format", "Format:",
                  choices = c("CSV" = "csv", "Excel (.xlsx)" = "xlsx", "RDS (R)" = "rds", "JSON" = "json", "TSV" = "tsv"),
                  selected = "csv"
                ),
                checkboxInput("export_transformed", "Export transformed data", value = FALSE),
                downloadButton("download_data", "Download Data", class = "btn-primary btn-block"),
                hr(),
                div(class = "section-title", tags$i(class = "fas fa-file-pdf"), "Export Reports"),
                selectInput("report_type", "Report Type:",
                  choices = c("Full EDA Report (HTML)" = "eda_html", "Summary Statistics (CSV)" = "stats_csv", "Correlation Matrix (CSV)" = "corr_csv", "Variable Profile (HTML)" = "profile_html"),
                  selected = "eda_html"
                ),
                downloadButton("download_report", "Download Report", class = "btn-secondary btn-block"),
                hr(),
                div(class = "section-title", tags$i(class = "fas fa-chart-bar"), "Export Charts"),
                selectInput("chart_format", "Chart Format:",
                  choices = c("PNG (High-res)" = "png", "SVG (Vector)" = "svg", "HTML (Interactive)" = "html", "PDF" = "pdf"),
                  selected = "png"
                ),
                fluidRow(
                  column(6, numericInput("chart_export_width", "Width (in)", value = 6, min = 3, max = 20, step = 0.5)),
                  column(6, numericInput("chart_export_height", "Height (in)", value = 4, min = 2, max = 20, step = 0.5))
                ),
                numericInput("chart_export_dpi", "Resolution (DPI)", value = 300, min = 150, max = 600, step = 50),
                downloadButton("download_chart", "Download Current Chart", class = "btn-secondary btn-block")
              )
            ),
            column(8,
              div(class = "chart-container",
                h5(tags$i(class = "fas fa-eye"), "Data Preview"),
                p(class = "text-muted", "Preview of data to be exported:"),
                DTOutput("export_preview", height = "300px")
              ),
              div(class = "chart-container",
                h5(tags$i(class = "fas fa-info-circle"), "Export Summary"),
                uiOutput("export_summary")
              ),
              div(class = "chart-container",
                h5(tags$i(class = "fas fa-history"), "Export History"),
                uiOutput("export_history")
              )
            )
          )
        ),

        # ═══════════════════════════════════════════════════════════════════
        # TAB 7: INTERACTIVE DASHBOARDS
        # ═══════════════════════════════════════════════════════════════════
        tabPanel(
          title = tagList(icon("th"), "Dashboards"),
          value = "dashboards",
          fluidRow(
            column(3,
              div(class = "sidebar-section", style = "margin-top: 0;",
                div(class = "section-title", tags$i(class = "fas fa-layer-group"), "Dashboard Templates"),
                selectInput("dashboard_template", "Select Template:",
                  choices = c(
                    "Executive Summary" = "executive",
                    "Deep Dive Analysis" = "analytical",
                    "Side-by-Side Comparison" = "comparison",
                    "Time Series Dashboard" = "timeseries"
                  ),
                  selected = "executive"
                ),
                actionButton("load_dashboard_template", "Load Template", icon = icon("download"), class = "btn-primary btn-block"),
                hr(),
                div(class = "section-title", tags$i(class = "fas fa-save"), "Save & Export"),
                textInput("dashboard_name", "Dashboard Name:", value = "My Dashboard"),
                downloadButton("download_dashboard_config", "Save Config (JSON)", class = "btn-secondary btn-block mt-2"),
                downloadButton("download_dashboard_html", "Export as HTML", class = "btn-success btn-block mt-2"),
                hr(),
                div(class = "section-title", tags$i(class = "fas fa-upload"), "Load Configuration"),
                fileInput("upload_dashboard_config", "Upload Config File:", accept = ".json"),
                actionButton("apply_dashboard_config", "Apply Config", icon = icon("check"), class = "btn-secondary btn-block"),
                hr(),
                div(class = "section-title", tags$i(class = "fas fa-sync"), "Refresh"),
                checkboxInput("dashboard_auto_refresh", "Auto-refresh (every 30s)", value = FALSE),
                actionButton("refresh_dashboard", "Refresh Now", icon = icon("sync"), class = "btn-secondary btn-block")
              )
            ),
            column(9,
              div(class = "chart-container",
                div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 16px;",
                  h5(tags$i(class = "fas fa-chart-area"), "Dashboard Preview"),
                  div(
                    actionButton("dashboard_fullscreen", "", icon = icon("expand"), class = "btn-secondary btn-sm"),
                    actionButton("edit_dashboard_layout", "Edit Layout", icon = icon("edit"), class = "btn-secondary btn-sm ml-2")
                  )
                ),
                uiOutput("dashboard_content")
              ),
              div(class = "chart-container",
                h5(tags$i(class = "fas fa-info-circle"), "Template Info"),
                uiOutput("dashboard_template_info")
              )
            )
          )
        ),

        # ═══════════════════════════════════════════════════════════════════
        # ═══════════════════════════════════════════════════════════════════
        # TAB 8: OUTLIER DETECTION
        # ═══════════════════════════════════════════════════════════════════
        tabPanel(
          title = tagList(icon("exclamation-triangle"), "Outliers"),
          value = "outliers",
          fluidRow(
            column(4,
              div(class = "sidebar-section", style = "margin-top: 0;",
                div(class = "section-title", tags$i(class = "fas fa-search"), "Detection Settings"),
                selectInput("outlier_var", "Variable:", choices = NULL),
                selectInput("outlier_method", "Method:",
                  choices = c("IQR (1.5x)" = "iqr", "IQR (3x - Extreme)" = "iqr3", "Z-Score (> 2)" = "zscore2", "Z-Score (> 3)" = "zscore3", "MAD (Median Abs Dev)" = "mad", "Isolation Forest" = "iforest"),
                  selected = "iqr"
                ),
                actionButton("detect_outliers_btn", "Detect Outliers", icon = icon("search"), class = "btn-primary btn-block"),
                hr(),
                div(class = "section-title", tags$i(class = "fas fa-tools"), "Handle Outliers"),
                selectInput("outlier_action", "Action:",
                  choices = c("Remove Outliers" = "remove", "Cap to Bounds" = "cap", "Replace with NA" = "na", "Replace with Median" = "median", "Replace with Mean" = "mean"),
                  selected = "remove"
                ),
                actionButton("apply_outlier_action", "Apply Action", icon = icon("check"), class = "btn-success btn-block"),
                actionButton("reset_outliers", "Reset Data", icon = icon("undo"), class = "btn-secondary btn-block mt-2")
              )
            ),
            column(8,
              div(class = "stat-cards",
                div(class = "stat-card", 
                  div(class = "label", "Total Outliers"),
                  uiOutput("outlier_count")
                ),
                div(class = "stat-card",
                  div(class = "label", "% of Data"),
                  uiOutput("outlier_pct")
                ),
                div(class = "stat-card",
                  div(class = "label", "Lower Bound"),
                  uiOutput("outlier_lower")
                ),
                div(class = "stat-card",
                  div(class = "label", "Upper Bound"),
                  uiOutput("outlier_upper")
                )
              ),
              div(class = "chart-container",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  h5(tags$i(class = "fas fa-chart-bar"), "Outlier Visualization"),
                  actionButton("expand_outlier_chart", "", icon = icon("expand"), class = "btn-secondary btn-sm")
                ),
                plotlyOutput("outlier_plot", height = "400px")
              ),
              div(class = "chart-container",
                h5(tags$i(class = "fas fa-table"), "Outlier Details"),
                DTOutput("outlier_table", height = "300px")
              )
            )
          )
        )
      )
    )
  ),

  # ═══════════════════════════════════════════════════════════════════════════════
  # LOADING OVERLAY
  # ═══════════════════════════════════════════════════════════════════════════════
  conditionalPanel("input.loading",
    div(class = "loading-overlay",
      div(class = "loading-spinner"),
      div(class = "loading-text", "Processing...")
    )
  ),

  # ═══════════════════════════════════════════════════════════════════════════════
  # FOOTER
  # ═══════════════════════════════════════════════════════════════════════════════
  div(class = "app-footer",
    div(class = "footer-inner",
      span(class = "badge-accent", "v2.1"),
      span(class = "footer-text", "DataExplorerPro"),
      span(class = "footer-sep"),
      tags$i(class = "fas fa-brain", style = "color: var(--accent-primary); font-size: 11px;"),
      span(class = "footer-text", "Powered by Ollama"),
      span(class = "footer-sep"),
      tags$a(href = "#", tags$i(class = "fas fa-book", style = "margin-right: 4px;"), "Docs"),
      span(class = "footer-sep"),
      tags$a(href = "#", tags$i(class = "fab fa-github", style = "margin-right: 4px;"), "GitHub")
    )
  ),

  # ═══════════════════════════════════════════════════════════════════════════════
  # JAVASCRIPT
  # ═══════════════════════════════════════════════════════════════════════════════
  includeScript("www/app.js")
)

