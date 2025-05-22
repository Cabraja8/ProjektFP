import React, { useState, useEffect } from 'react';
import {
  ResponsiveContainer,
  LineChart, Line,
  BarChart, Bar,
  PieChart, Pie, Cell,
  XAxis, YAxis, Tooltip,
  AreaChart, Area,
  RadarChart, Radar,
  PolarGrid, PolarAngleAxis, PolarRadiusAxis,
  CartesianGrid, Legend
} from 'recharts';
import './App.css';
import Navbar from './Navbar';
import Footer from './Footer';
import CarouselSection from './Info';

export default function App() {
  const [data, setData] = useState([]);
  const [chartType, setChartType] = useState('line');
  const [intervalSec, setIntervalSec] = useState(2);
  const [source, setSource] = useState('all');
  const [startDate, setStartDate] = useState('');
  const [endDate, setEndDate] = useState('');
  const [activeSeries, setActiveSeries] = useState([]);

  const fetchData = () => {
    fetch(`/data?source=${source}`)
      .then(res => res.json())
      .then(json => {
        const parsed = json.map(item => ({
          ...item,
          timestamp: new Date(item.timestamp).toISOString(),
          displayTime: new Date(item.timestamp).toLocaleTimeString(),
          value: Number(item.value),
          uv: Number(item.uv || item.value), // dodatno ako koristiš više serija
          pv: Number(item.pv || item.value)
        }));
        setData(parsed);
      })
      .catch(console.error);
  };

  useEffect(() => {
    fetchData();
    let timer;
    if (intervalSec > 0) {
      timer = setInterval(fetchData, intervalSec * 1000);
    }
    return () => clearInterval(timer);
  }, [intervalSec, source]);

  const filteredData = data.filter(entry => {
    const entryTime = new Date(entry.timestamp).getTime();
    const fromTime = startDate ? new Date(startDate).getTime() : -Infinity;
    const toTime = endDate ? new Date(endDate).getTime() : Infinity;
    return entryTime >= fromTime && entryTime <= toTime;
  });

  const totalValue = filteredData.reduce((acc, curr) => acc + curr.value, 0);

  const handleLegendClick = (dataKey) => {
    if (activeSeries.includes(dataKey)) {
      setActiveSeries(activeSeries.filter(el => el !== dataKey));
    } else {
      setActiveSeries(prev => [...prev, dataKey]);
    }
  };

  return (
    <div className="container">
      <div className="app-container d-flex flex-column min-vh-100">
        <Navbar />
        <CarouselSection />

        <div className="navbar bg-light px-4 mb-4 rounded shadow-sm">
          <form className="row g-3 align-items-center w-100">
            <div className="col-md-4">
              <label className="form-label mb-0">Izvor:</label>
              <select className="form-select form-select-sm" value={source} onChange={e => setSource(e.target.value)}>
                <option value="all">Svi</option>
                <option value="AAPL">Stock AAPL</option>
                <option value="BTCUSDT">Crypto BTC</option>
              </select>
            </div>

            <div className="col-md-4">
              <label className="form-label mb-0">Tip grafikona:</label>
              <select className="form-select form-select-sm" value={chartType} onChange={e => setChartType(e.target.value)}>
                <option value="line">Linijski</option>
                <option value="bar">Stupčasti</option>
                <option value="pie">Tortni</option>
                <option value="area">Površinski</option>
                <option value="radar">Radar</option>
              </select>
            </div>

            <div className="col-md-4">
              <label className="form-label mb-0">Osvježavanje:</label>
              <select
                className="form-select form-select-sm"
                value={intervalSec}
                onChange={e => setIntervalSec(Number(e.target.value))}
              >
                <option value={60}>1 min</option>
                <option value={2}>2 sec</option>
                <option value={0}>Ručno</option>
              </select>
            </div>

            <div className="col-md-4 mt-3">
              <label className="form-label mb-0">Od:</label>
              <input
                type="datetime-local"
                className="form-control form-control-sm"
                value={startDate}
                onChange={e => setStartDate(e.target.value)}
              />
            </div>

            <div className="col-md-4 mt-3">
              <label className="form-label mb-0">Do:</label>
              <input
                type="datetime-local"
                className="form-control form-control-sm"
                value={endDate}
                onChange={e => setEndDate(e.target.value)}
              />
            </div>

            <div className="ms-auto d-md-flex justify-content-md-end">
              {intervalSec === 0 && (
                <button type="button" onClick={fetchData}>
                  Osvježi
                </button>
              )}
            </div>
          </form>
        </div>

        <div className="chart-wrapper">
<ResponsiveContainer width="100%" height={400}>
  {chartType === 'line' && (
    <LineChart data={filteredData}>
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
      <Tooltip />
      <Legend
        payload={[
          { value: 'UV', type: 'circle', color: '#8884d8', dataKey: 'uv' },
          { value: 'PV', type: 'circle', color: '#82ca9d', dataKey: 'pv' }
        ]}
        onClick={e => handleLegendClick(e.dataKey)}
      />
      <Line hide={activeSeries.includes('uv')} type="monotone" dataKey="uv" stroke="#8884d8" />
      <Line hide={activeSeries.includes('pv')} type="monotone" dataKey="pv" stroke="#82ca9d" />
    </LineChart>
  )}

  {chartType === 'bar' && (
    <BarChart data={filteredData}>
      <CartesianGrid strokeDasharray="3 3" />
      <XAxis dataKey="displayTime" />
      <YAxis />
      <Tooltip />
      <Legend
        payload={[
          { value: 'UV', type: 'square', color: '#8884d8', dataKey: 'uv' },
          { value: 'PV', type: 'square', color: '#82ca9d', dataKey: 'pv' }
        ]}
        onClick={e => handleLegendClick(e.dataKey)}
      />
      <Bar hide={activeSeries.includes('uv')} dataKey="uv" fill="#8884d8" />
      <Bar hide={activeSeries.includes('pv')} dataKey="pv" fill="#82ca9d" />
    </BarChart>
  )}

  {chartType === 'pie' && (
    <PieChart>
      <Pie
        data={filteredData.slice(-5)}
        dataKey="value"
        nameKey="displayTime"
        outerRadius={80}
        label
      >
        {filteredData.slice(-5).map((entry, i) => (
          <Cell key={`cell-${i}`} fill={entry.color || ['#8884d8', '#82ca9d', '#ffc658', '#d0ed57', '#a4de6c'][i % 5]} />
        ))}
      </Pie>
      <Tooltip />
      <Legend />
    </PieChart>
  )}

  {chartType === 'area' && (
    <AreaChart data={filteredData}>
      <CartesianGrid strokeDasharray="3 3" />
      <XAxis dataKey="displayTime" />
      <YAxis />
      <Tooltip />
      <Legend
        payload={[
          { value: 'UV', type: 'square', color: '#8884d8', dataKey: 'uv' },
          { value: 'PV', type: 'square', color: '#82ca9d', dataKey: 'pv' }
        ]}
        onClick={e => handleLegendClick(e.dataKey)}
      />
      <Area hide={activeSeries.includes('uv')} type="monotone" dataKey="uv" stroke="#8884d8" fill="#8884d8" />
      <Area hide={activeSeries.includes('pv')} type="monotone" dataKey="pv" stroke="#82ca9d" fill="#82ca9d" />
    </AreaChart>
  )}

  {chartType === 'radar' && (
    <RadarChart outerRadius={90} width={500} height={400} data={filteredData.map((entry, index) => ({
      ...entry,
      displayTime: entry.displayTime || `Vrijeme ${index + 1}`,
    }))}>
      <PolarGrid />
      <PolarAngleAxis dataKey="displayTime" />
      <PolarRadiusAxis />
      <Radar
        name="UV"
        dataKey="uv"
        stroke="#8884d8"
        fill="#8884d8"
        fillOpacity={0.6}
        hide={activeSeries.includes('uv')}
      />
      <Radar
        name="PV"
        dataKey="pv"
        stroke="#82ca9d"
        fill="#82ca9d"
        fillOpacity={0.6}
        hide={activeSeries.includes('pv')}
      />
      <Tooltip />
      <Legend
        payload={[
          { value: 'UV', type: 'circle', color: '#8884d8', dataKey: 'uv' },
          { value: 'PV', type: 'circle', color: '#82ca9d', dataKey: 'pv' }
        ]}
        onClick={e => handleLegendClick(e.dataKey)}
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
