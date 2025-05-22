import React from 'react';


const CarouselSection = () => {
  return (
    <div className="container mt-5 mb-5">
      <div className="row">
        {/* Info Section (3 columns) */}
        <div className="col-md-3">
          <h3>Informacija</h3>
<div className="description p-4 bg-light rounded shadow-sm">
  <p className="lead fw-semibold mb-3">
    Ova aplikacija predstavlja <strong>jednostavan alat za analizu burzovnih dionica u stvarnom vremenu</strong>.  
    Razvijena je korištenjem <span className="text-primary fw-bold">funkcijskog programiranja</span> u programskom jeziku <span className="text-success fw-bold">Haskell</span>.
  </p>
  <p className="mb-4 fs-5">
    Cilj projekta je omogućiti korisnicima <strong>uvide u aktualna tržišna kretanja</strong> putem <em>intuitivnog i učinkovitog sučelja</em>.
  </p>
  <div className="mb-3">
    <h5 className="text-secondary fw-bold mb-2">Tim:</h5>
    <ul className="list-unstyled ms-3">
      <li><strong>Ivan Čabraja</strong></li>
      <li><strong>Erik Mirković</strong></li>
      <li><strong>Matej Kurevija</strong></li>
    </ul>
  </div>
  <div>
    <h5 className="text-secondary fw-bold mb-2">Mentor:</h5>
    <p className="ms-3"><strong>doc. dr. sc. Siniša Miličić</strong></p>
  </div>
</div>


        </div>

        <div className="col-md-9">
       <div id="carouselExampleSlidesOnly" className="carousel slide" data-bs-ride="carousel">
  <div className="carousel-inner">
    <div className="carousel-item active">
      <img className="d-block w-100" src="/Imgs/First.jpg" alt="First slide" />

    </div>
    <div className="carousel-item">
      <img className="d-block w-100" src="/Imgs/FP.png" alt="Second slide" />
    </div>
    <div className="carousel-item">
      <img className="d-block w-100" src="/Imgs/Haskell.png" alt="Third slide" />
    </div>
  </div>
</div>
        </div>
      </div>
    </div>
  );
};

export default CarouselSection;

