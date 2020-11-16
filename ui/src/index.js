import style from './style.css';
import charts from './charts';

document.addEventListener('DOMContentLoaded', () => {
  google.charts.load('current', { 'packages': ['timeline', 'calendar'] });

  charts();
});
