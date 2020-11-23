import style from './style.css';
import charts from './charts';
import { setUserProfile } from './user.js';

document.addEventListener('DOMContentLoaded', () => {
  google.charts.load('current', { 'packages': ['corechart', 'bar', 'timeline', 'calendar'] });

  setUserProfile();
  charts();
});
