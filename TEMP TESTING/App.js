// src/App.js
import React, { useState, useEffect } from 'react';

function App() {
  const [teamName, setTeamName] = useState('Arsenal');
  const [teams, setTeams] = useState([]);

  const fetchTeams = () => {
    fetch(`/v1/searchteams?t=${encodeURIComponent(teamName)}`)
      .then(res => res.json())
      .then(data => {
        // data.teams je niz timova iz API-ja
        setTeams(data.teams || []);
      })
      .catch(err => console.error('Fetch error:', err));
  };

  useEffect(() => {
    fetchTeams();
  }, []);

  return (
    <div>
      <h1>Search Arsenal & Friends</h1>
      <input
        value={teamName}
        onChange={e => setTeamName(e.target.value)}
        placeholder="Team name"
      />
      <button onClick={fetchTeams}>Search</button>

      <div>
        {teams.map(team => (
          <div key={team.idTeam}>
            <h2>{team.strTeam}</h2>
            <img src={team.strBadge} alt={`${team.strTeam} badge`} width={80}/>
            <p>Stadion: {team.strStadium}</p>
            <p>Kapacitet: {team.intStadiumCapacity}</p>
          </div>
        ))}
      </div>
    </div>
  );
}

export default App;
