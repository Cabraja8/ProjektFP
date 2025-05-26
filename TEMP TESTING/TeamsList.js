import React from 'react';
import PropTypes from 'prop-types';

function TeamsList({ teams }) {
  return (
    <div className="row">
      {teams.map(team => (
        <div key={team.id} className="col-md-4 mb-4">
          <div className="card h-100 shadow-sm">
            <img src={team.badge} alt={`${team.name} badge`} className="card-img-top p-3" />
            <div className="card-body d-flex flex-column">
              <h5 className="card-title">{team.name}</h5>
              <p><strong>Capacity:</strong> {team.capacity}</p>
            </div>
          </div>
        </div>
      ))}
    </div>
  );
}

TeamsList.propTypes = {
  teams: PropTypes.arrayOf(
    PropTypes.shape({
      id: PropTypes.string.isRequired,
      name: PropTypes.string.isRequired,
      badge: PropTypes.string,
      capacity: PropTypes.number.isRequired,
    })
  ).isRequired,
};

export default TeamsList;