import React from 'react';

const Footer = () => {
  return (
    <div className="container mt-auto">
      <footer className="py-3 my-4">
        <ul className="nav justify-content-center border-bottom pb-3 mb-3">
        
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
        <p className="text-center text-body-secondary">© 2030 PR-FP Real-time data</p>
      </footer>
    </div>
  );
};

export default Footer;
