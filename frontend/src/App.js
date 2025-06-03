import React, { useState, useEffect } from 'react';
import Navbar from './Navbar';
import Footer from './Footer';
import CarouselSection from './Info';

import {
  ResponsiveContainer,
  LineChart, Line,
  CartesianGrid,
  XAxis, YAxis,
  Tooltip,
  Legend,
  BarChart, Bar,
  PieChart, Pie, Cell,
  AreaChart, Area,
  RadarChart, Radar,
  PolarGrid, PolarAngleAxis, PolarRadiusAxis
} from 'recharts';
export default function App() {
  const [rawData, setRawData] = useState([]);
  const [data, setData]     = useState([]);
  const [chartType, setChartType] = useState('line');

  // intervalSec == 0 → ručno; intervalSec > 0 → broj sekundi
  const [intervalSec, setIntervalSec] = useState(300);

  const [source, setSource] = useState('all');
  const [countries, setCountries] = useState([]);
  const [selectedCountry, setSelectedCountry] = useState('all');

  const [startDate, setStartDate] = useState('');
  const [endDate, setEndDate]     = useState('');

  // Vremenski filtri (HH:MM)
  const [startTime, setStartTime] = useState('');
  const [endTime, setEndTime]     = useState('');

  const [activeSeries, setActiveSeries] = useState([]);

  // Kad se izvor promijeni na “earthquake”, resetiraj državu
  useEffect(() => {
    if (source === 'earthquake') {
      setSelectedCountry('all');
    }
    // Ne diraj intervalSec ovdje—pusti korisnika da sam odabere.
  }, [source]);

  // Glavni efekt samo ovisi o intervalSec i source.
  // Ako je intervalSec > 0, postavi setInterval; inače ne.
  useEffect(() => {
    // Prvo odmah dohvati podatke
    fetchData();

    if (intervalSec > 0) {
      const timer = setInterval(fetchData, intervalSec * 1000);
      return () => clearInterval(timer);
    }
    // Ako je intervalSec = 0, ne pokrećemo nijedan timer.
  }, [intervalSec, source]);

  const fetchData = () => {
    let url = `/data?source=${source}`;
    if (source === 'earthquake') {
      url += `&country=all`;
    }

    fetch(url)
      .then(res => res.json())
      .then(json => {
        if (source === 'earthquake') {
          const enriched = json.map(item => {
            const timestampISO = new Date(item.timestamp).toISOString();
            const displayTime  = new Date(item.timestamp).toLocaleTimeString();
            const valueNum     = Number(item.value);
            const placeStr     = item.place || '';
            const parts        = placeStr.split(',').map(p => p.trim());
            const statePart    = parts.length > 1 ? parts[parts.length - 1] : '';
            return {
              ...item,
              timestamp: timestampISO,
              displayTime,
              value: valueNum,
              uv: Number(item.uv || valueNum),
              pv: Number(item.pv || valueNum),
              place: placeStr,
              state: statePart
            };
          });

          const stateSet = new Set();
          enriched.forEach(d => {
            if (d.state) stateSet.add(d.state);
          });
          setRawData(enriched);
          setCountries(['all', ...Array.from(stateSet).sort()]);
        } else {
          const mapped = json.map(item => {
            const timestampISO = new Date(item.timestamp).toISOString();
            const displayTime  = new Date(item.timestamp).toLocaleTimeString();
            const valueNum     = Number(item.value);
            return {
              ...item,
              timestamp: timestampISO,
              displayTime,
              value: valueNum,
              uv: Number(item.uv || valueNum),
              pv: Number(item.pv || valueNum),
              place: '',
              state: ''
            };
          });
          setRawData(mapped);
          setCountries(['all']);
          setSelectedCountry('all');
        }
      })
      .catch(console.error);
  };

  // Filtriranje podataka
  useEffect(() => {
    let filtered = rawData;

    if (source === 'earthquake' && selectedCountry !== 'all') {
      filtered = filtered.filter(d => d.state === selectedCountry);
    }

    if (startDate || endDate) {
      filtered = filtered.filter(entry => {
        const ts = new Date(entry.timestamp).getTime();
        const from = startDate ? new Date(startDate).getTime() : -Infinity;
        const to   = endDate   ? new Date(endDate).getTime()   : Infinity;
        return ts >= from && ts <= to;
      });
    }

    if (startTime || endTime) {
      filtered = filtered.filter(entry => {
        const dt = new Date(entry.timestamp);
        const minutes = dt.getHours() * 60 + dt.getMinutes();

        const [sh, sm] = startTime 
          ? startTime.split(':').map(x => parseInt(x, 10))
          : [0, 0];
        const [eh, em] = endTime 
          ? endTime.split(':').map(x => parseInt(x, 10))
          : [23, 59];

        const fromMin = sh * 60 + sm;
        const toMin   = eh * 60 + em;
        return minutes >= fromMin && minutes <= toMin;
      });
    }

    setData(filtered);
  }, [
    rawData,
    source,
    selectedCountry,
    startDate,
    endDate,
    startTime,
    endTime
  ]);

  const totalValue = data.reduce((acc, curr) => acc + curr.value, 0);

 

  return (
    <div className="container">
      <div className="app-container d-flex flex-column min-vh-100">
        <Navbar />
        <CarouselSection />

        <div className="navbar bg-light px-4 mb-4 rounded shadow-sm">
          <form className="row g-3 align-items-center w-100"
                onSubmit={e => e.preventDefault()}>

            {/* IZVOR PODATAKA */}
            <div className="col-md-3">
              <label className="form-label mb-0">Izvor:</label>
              <select
                className="form-select form-select-sm"
                value={source}
                onChange={e => {
                  setSource(e.target.value);
                  setStartDate('');
                  setEndDate('');
                  setStartTime('');
                  setEndTime('');
                }}
              >
                <option value="all">Svi (Dummy)</option>
                <option value="AAPL">Stock AAPL (Dummy)</option>
                <option value="BTCUSDT">Crypto BTC (Dummy)</option>
                <option value="earthquake">Seizmički</option>
              </select>
            </div>

            {/* DROPDOWN DRŽAVA/REGIJA – samo za earthquake */}
            {source === 'earthquake' && (
              <div className="col-md-3">
                <label className="form-label mb-0">Država/Regija:</label>
                <select
                  className="form-select form-select-sm"
                  value={selectedCountry}
                  onChange={e => setSelectedCountry(e.target.value)}
                >
                  {countries.map(stateName => (
                    <option key={stateName} value={stateName}>
                      {stateName === 'all' ? 'Sve regije' : stateName}
                    </option>
                  ))}
                </select>
              </div>
            )}

            {/* TIP GRAFIKONA */}
            <div className={`col-md-${source === 'earthquake' ? '2' : '3'}`}>
              <label className="form-label mb-0">Tip grafikona:</label>
              <select
                className="form-select form-select-sm"
                value={chartType}
                onChange={e => setChartType(e.target.value)}
              >
                <option value="line">Linijski</option>
                <option value="bar">Stupčasti</option>
                <option value="pie">Tortni</option>
                <option value="area">Površinski</option>
                <option value="radar">Radar</option>
              </select>
            </div>

            {/* OSVJEŽAVANJE (5 min, 1 min, Ručno) */}
            <div className="col-md-2">
              <label className="form-label mb-0">Osvježavanje:</label>
              <select
                className="form-select form-select-sm"
                value={intervalSec}
                onChange={e => {
                  const v = Number(e.target.value);
                  setIntervalSec(v);
                  if (v > 0) {
                    // Očisti vremenski filter kad idemo na automatsko osvježavanje
                    setStartTime('');
                    setEndTime('');
                  }
                }}
              >
                <option value={300}>5 min</option>
                <option value={60}>1 min</option>
                <option value={0}>Ručno</option>
              </select>
            </div>

            {/* FILTRIRANJE PO DATUMU */}
            <div className="col-md-4 mt-3">
              <label className="form-label mb-0">Od (datum-vrijeme):</label>
              <input
                type="datetime-local"
                className="form-control form-control-sm"
                value={startDate}
                onChange={e => setStartDate(e.target.value)}
              />
            </div>
            <div className="col-md-4 mt-3">
              <label className="form-label mb-0">Do (datum-vrijeme):</label>
              <input
                type="datetime-local"
                className="form-control form-control-sm"
                value={endDate}
                onChange={e => setEndDate(e.target.value)}
              />
            </div>

            {/* FILTRIRANJE PO VREMENU (HH:MM) */}
            <div className="col-md-3 mt-3">
              <label className="form-label mb-0">Od (vrijeme):</label>
              <input
                type="time"
                className="form-control form-control-sm"
                value={startTime}
                onChange={e => setStartTime(e.target.value)}
              />
            </div>
            <div className="col-md-3 mt-3">
              <label className="form-label mb-0">Do (vrijeme):</label>
              <input
                type="time"
                className="form-control form-control-sm"
                value={endTime}
                onChange={e => setEndTime(e.target.value)}
              />
            </div>

            {/* GUMB ZA RUČNO OSVJEŽAVANJE */}
            <div className="ms-auto d-md-flex justify-content-md-end">
              {intervalSec === 0 && (
                <button type="button" onClick={fetchData}>
                  Osvježi
                </button>
              )}
            </div>
          </form>
        </div>

        {/* GRAFIČKI PRIKAZ */}
        <div className="chart-wrapper">
          <ResponsiveContainer width="100%" height={400}>

            {/* LINIJSKI GRAF */}
            {chartType === 'line' && (
              <LineChart data={data}>
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis
                  dataKey="displayTime"
                  tick={{ fontSize: 12 }}
                  angle={-45}
                  textAnchor="end"
                  interval="preserveStartEnd"
                  height={70}
                />
                <YAxis />
                <Tooltip
                  content={({ active, payload }) => {
                    if (active && payload && payload.length) {
                      const d = payload[0].payload;
                      return (
                        <div style={{ backgroundColor: '#fff', padding: 10, border: '1px solid #ccc' }}>
                          <p><strong>Vrijeme:</strong> {d.displayTime}</p>
                          <p><strong>Vrijednost:</strong> {d.value}</p>
                          {source === 'earthquake' && (
                            <>
                              <p><strong>Mjesto:</strong> {d.place}</p>
                              <p><strong>Država:</strong> {d.state}</p>
                            </>
                          )}
                        </div>
                      );
                    }
                    return null;
                  }}
                />
               
               <Line type="monotone" dataKey="value" stroke="#8884d8" />
              </LineChart>
            )}

            {/* STUPČASTI GRAF */}
            {chartType === 'bar' && (
              <BarChart data={data}>
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis dataKey="displayTime" />
                <YAxis />
                <Tooltip
                  content={({ active, payload }) => {
                    if (active && payload && payload.length) {
                      const d = payload[0].payload;
                      return (
                        <div style={{ backgroundColor: '#fff', padding: 10, border: '1px solid #ccc' }}>
                          <p><strong>Vrijeme:</strong> {d.displayTime}</p>
                          <p><strong>Vrijednost:</strong> {d.value}</p>
                          {source === 'earthquake' && (
                            <>
                              <p><strong>Mjesto:</strong> {d.place}</p>
                              <p><strong>Država:</strong> {d.state}</p>
                            </>
                          )}
                        </div>
                      );
                    }
                    return null;
                  }}
                />
                
              <Bar type="monotone" dataKey="value" stroke="#8884d8" />
              </BarChart>
            )}

            {/* TORTNI GRAF */}
            {chartType === 'pie' && (
              <PieChart>
                <Pie
                  data={data.slice(-5)}
                  dataKey="value"
                  nameKey="displayTime"
                  outerRadius={80}
                  label
                >
                  {data.slice(-5).map((entry, i) => (
                    <Cell
                      key={`cell-${i}`}
                      fill={['#8884d8', '#82ca9d', '#ffc658', '#d0ed57', '#a4de6c'][i % 5]}
                    />
                  ))}
                </Pie>
                <Tooltip
                  content={({ active, payload }) => {
                    if (active && payload && payload.length) {
                      const d = payload[0].payload;
                      return (
                        <div style={{ backgroundColor: '#fff', padding: 10, border: '1px solid #ccc' }}>
                          <p><strong>Vrijeme:</strong> {d.displayTime}</p>
                          <p><strong>Vrijednost:</strong> {d.value}</p>
                          {source === 'earthquake' && (
                            <>
                              <p><strong>Mjesto:</strong> {d.place}</p>
                              <p><strong>Država:</strong> {d.state}</p>
                            </>
                          )}
                        </div>
                      );
                    }
                    return null;
                  }}
                />
                <Legend />
              </PieChart>
            )}

            {/* POVRŠINSKI GRAF */}
            {chartType === 'area' && (
              <AreaChart data={data}>
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis dataKey="displayTime" />
                <YAxis />
                <Tooltip
                  content={({ active, payload }) => {
                    if (active && payload && payload.length) {
                      const d = payload[0].payload;
                      return (
                        <div style={{ backgroundColor: '#fff', padding: 10, border: '1px solid #ccc' }}>
                          <p><strong>Vrijeme:</strong> {d.displayTime}</p>
                          <p><strong>Vrijednost:</strong> {d.value}</p>
                          {source === 'earthquake' && (
                            <>
                              <p><strong>Mjesto:</strong> {d.place}</p>
                              <p><strong>Država:</strong> {d.state}</p>
                            </>
                          )}
                        </div>
                      );
                    }
                    return null;
                  }}
                />
                
               <Area type="monotone" dataKey="value" stroke="#8884d8" />
              </AreaChart>
            )}

            {/* RADAR GRAF */}
            {chartType === 'radar' && (
              <RadarChart
                outerRadius={90}
                width={500}
                height={400}
                data={data.map((entry, index) => ({
                  ...entry,
                  displayTime: entry.displayTime || `Vrijeme ${index + 1}`
                }))}
              >
                <PolarGrid />
                <PolarAngleAxis dataKey="displayTime" />
                <PolarRadiusAxis />
              <Radar type="monotone" dataKey="value" stroke="#8884d8" />
                <Tooltip
                  content={({ active, payload }) => {
                    if (active && payload && payload.length) {
                      const d = payload[0].payload;
                      return (
                        <div style={{ backgroundColor: '#fff', padding: 10, border: '1px solid #ccc' }}>
                          <p><strong>Vrijeme:</strong> {d.displayTime}</p>
                          <p><strong>Vrijednost:</strong> {d.value}</p>
                          {source === 'earthquake' && (
                            <>
                              <p><strong>Mjesto:</strong> {d.place}</p>
                              <p><strong>Država:</strong> {d.state}</p>
                            </>
                          )}
                        </div>
                      );
                    }
                    return null;
                  }}
                />
                
              </RadarChart>
            )}
          </ResponsiveContainer>

          <div className="text-end mt-2">
            <strong>Ukupna vrijednost (filtrirano):</strong> {totalValue.toFixed(2)}
          </div>
        </div>

        <Footer />
      </div>
    </div>
  );
}
