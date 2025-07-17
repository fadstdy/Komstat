/* Custom CSS for Dashboard Analisis Statistik */
  /* www/custom.css */
  
  /* Main dashboard styling */
  .content-wrapper, .right-side {
    background-color: #f8f9fa;
  }

/* Box styling */
  .box {
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    margin-bottom: 20px;
  }

.box-header {
  border-radius: 8px 8px 0 0;
  padding: 15px;
}

.box-body {
  padding: 20px;
}

/* Button styling */
  .btn {
    border-radius: 6px;
    font-weight: 500;
    padding: 8px 16px;
    margin: 2px;
  }

.btn-primary {
  background-color: #007bff;
    border-color: #007bff;
}

.btn-primary:hover {
  background-color: #0056b3;
    border-color: #0056b3;
}

.btn-success {
  background-color: #28a745;
    border-color: #28a745;
}

.btn-success:hover {
  background-color: #1e7e34;
    border-color: #1e7e34;
}

.btn-warning {
  background-color: #ffc107;
    border-color: #ffc107;
    color: #212529;
}

.btn-warning:hover {
  background-color: #d39e00;
    border-color: #d39e00;
    color: #212529;
}

.btn-info {
  background-color: #17a2b8;
    border-color: #17a2b8;
}

.btn-info:hover {
  background-color: #117a8b;
    border-color: #117a8b;
}

/* Form controls */
  .form-control {
    border-radius: 4px;
    border: 1px solid #ced4da;
    padding: 8px 12px;
  }

.form-control:focus {
  border-color: #007bff;
    box-shadow: 0 0 0 0.2rem rgba(0,123,255,0.25);
}

/* Select inputs */
  .selectize-control .selectize-input {
    border-radius: 4px;
    border: 1px solid #ced4da;
    padding: 8px 12px;
  }

.selectize-control .selectize-input:focus {
  border-color: #007bff;
    box-shadow: 0 0 0 0.2rem rgba(0,123,255,0.25);
}

/* DataTable styling */
  .dataTables_wrapper {
    margin-top: 20px;
  }

.dataTables_wrapper .dataTables_length select {
  padding: 4px 8px;
  border-radius: 4px;
  border: 1px solid #ced4da;
}

.dataTables_wrapper .dataTables_filter input {
  padding: 4px 8px;
  border-radius: 4px;
  border: 1px solid #ced4da;
}

.dataTable {
  border-collapse: separate;
  border-spacing: 0;
  border-radius: 6px;
  overflow: hidden;
}

.dataTable thead th {
  background-color: #f8f9fa;
    border-bottom: 2px solid #dee2e6;
  font-weight: 600;
  padding: 12px;
}

.dataTable tbody tr:nth-child(even) {
  background-color: #f8f9fa;
}

.dataTable tbody tr:hover {
  background-color: #e9ecef;
}

.dataTable tbody td {
  padding: 10px 12px;
  border-bottom: 1px solid #dee2e6;
}

/* Alert styling */
  .alert {
    border-radius: 6px;
    padding: 15px;
    margin-bottom: 20px;
  }

.alert-info {
  background-color: #d1ecf1;
    border-color: #bee5eb;
    color: #0c5460;
}

.alert-success {
  background-color: #d4edda;
    border-color: #c3e6cb;
    color: #155724;
}

.alert-warning {
  background-color: #fff3cd;
    border-color: #ffeaa7;
    color: #856404;
}

.alert-danger {
  background-color: #f8d7da;
    border-color: #f5c6cb;
    color: #721c24;
}

/* Navigation tabs */
  .nav-tabs {
    border-bottom: 2px solid #dee2e6;
  }

.nav-tabs .nav-link {
  border-radius: 6px 6px 0 0;
  border: 1px solid transparent;
  padding: 10px 15px;
  color: #495057;
    font-weight: 500;
}

.nav-tabs .nav-link:hover {
  border-color: #e9ecef #e9ecef #dee2e6;
    background-color: #f8f9fa;
}

.nav-tabs .nav-link.active {
  color: #007bff;
    background-color: #fff;
    border-color: #dee2e6 #dee2e6 #fff;
}

/* Sidebar styling */
  .sidebar {
    background-color: #343a40;
  }

.sidebar-menu > li > a {
  color: #c2c7d0;
    padding: 12px 15px;
  font-weight: 500;
}

.sidebar-menu > li > a:hover {
  background-color: #495057;
    color: #fff;
}

.sidebar-menu > li.active > a {
  background-color: #007bff;
    color: #fff;
}

/* Header styling */
  .main-header {
    background-color: #fff;
      border-bottom: 1px solid #dee2e6;
  }

.main-header .navbar {
  background-color: #007bff;
}

.main-header .navbar-brand {
  color: #fff;
    font-weight: 600;
  font-size: 18px;
}

/* Progress bars */
  .progress {
    height: 20px;
    border-radius: 10px;
    background-color: #e9ecef;
  }

.progress-bar {
  border-radius: 10px;
  font-weight: 500;
}

/* Spinner styling */
  .spinner {
    display: inline-block;
    width: 40px;
    height: 40px;
    border: 3px solid #f3f3f3;
    border-top: 3px solid #007bff;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}

/* Plot containers */
  .plot-container {
    background-color: #fff;
      border-radius: 6px;
    padding: 20px;
    margin: 10px 0;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }

/* Text output styling */
  .shiny-text-output {
    background-color: #f8f9fa;
      border: 1px solid #dee2e6;
    border-radius: 4px;
    padding: 15px;
    font-family: 'Courier New', monospace;
    white-space: pre-wrap;
    max-height: 400px;
    overflow-y: auto;
  }

/* Verbatim output styling */
  .shiny-verbatim-output {
    background-color: #f8f9fa;
      border: 1px solid #dee2e6;
    border-radius: 4px;
    padding: 15px;
    font-family: 'Courier New', monospace;
    white-space: pre-wrap;
    max-height: 400px;
    overflow-y: auto;
    font-size: 12px;
  }

/* Table styling */
  .table {
    border-radius: 6px;
    overflow: hidden;
  }

.table thead th {
  background-color: #007bff;
    color: #fff;
    border: none;
  font-weight: 600;
  padding: 12px;
}

.table tbody tr:nth-child(even) {
  background-color: #f8f9fa;
}

.table tbody tr:hover {
  background-color: #e9ecef;
}

.table tbody td {
  padding: 10px 12px;
  border-top: 1px solid #dee2e6;
}

/* Responsive design */
  @media (max-width: 768px) {
    .box {
      margin: 10px 5px;
    }
    
    .box-body {
      padding: 15px;
    }
    
    .btn {
      width: 100%;
      margin: 5px 0;
    }
    
    .dataTable {
      font-size: 12px;
    }
    
    .nav-tabs .nav-link {
      padding: 8px 10px;
      font-size: 12px;
    }
  }

/* Custom color scheme */
  :root {
    --primary-color: #007bff;
      --secondary-color: #6c757d;
      --success-color: #28a745;
      --info-color: #17a2b8;
      --warning-color: #ffc107;
      --danger-color: #dc3545;
      --light-color: #f8f9fa;
      --dark-color: #343a40;
  }

/* Animation effects */
  .fade-in {
    animation: fadeIn 0.5s ease-in;
  }

@keyframes fadeIn {
  from { opacity: 0; }
  to { opacity: 1; }
}

.slide-in {
  animation: slideIn 0.3s ease-out;
}

@keyframes slideIn {
  from { transform: translateX(-100%); }
  to { transform: translateX(0); }
}

/* Tooltip styling */
  .tooltip {
    font-size: 12px;
    background-color: #343a40;
      color: #fff;
      border-radius: 4px;
    padding: 5px 10px;
  }

/* Loading overlay */
  .loading-overlay {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: rgba(255, 255, 255, 0.8);
    display: flex;
    justify-content: center;
    align-items: center;
    z-index: 9999;
  }

/* Custom scrollbar */
  ::-webkit-scrollbar {
    width: 8px;
  }

::-webkit-scrollbar-track {
  background: #f1f1f1;
    border-radius: 4px;
}

::-webkit-scrollbar-thumb {
  background: #888;
    border-radius: 4px;
}

::-webkit-scrollbar-thumb:hover {
  background: #555;
}