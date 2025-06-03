import React from 'react';

const Navbar = () => {
  return (
    <div className="container">
      <header className="d-flex flex-wrap justify-content-center py-3 mb-4 border-bottom">
        <div className="d-flex align-items-center mb-3 mb-md-0 me-md-auto text-dark text-decoration-none">
          <svg className="bi me-2" width="40" height="32" aria-hidden="true">
            <use xlinkHref="#bootstrap" />
          </svg>
          <span className="fs-4"><h1>Real-Time Data</h1></span>
        </div>

        <ul className="nav nav-pills">
          <li className="nav-item">
            <span className="nav-link active">Dashboard</span>
          </li>
          <li className="nav-item">
            <a
              href="https://github.com/Cabraja8/ProjektFP"
              className="nav-link"
              target="_blank"
              rel="noopener noreferrer"
            >
              GitHub
            </a>
          </li>
           <li className="nav-item">
            <a
              href="https://gitlab2.unipu.hr/ivcabraja/fp-rtd/-/tree/main"
              className="nav-link"
              target="_blank"
              rel="noopener noreferrer"
            >
              Gitlab
            </a>
          </li>
        </ul>
      </header>
    </div>
  );
};

export default Navbar;

