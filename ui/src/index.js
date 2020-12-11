import style from './style.css';
import charts from './charts';
import logs from './logs';
import { setUserProfile } from './user.js';
import Navigo from 'navigo';

document.addEventListener('DOMContentLoaded', () => {
  google.charts.load('current', { 'packages': ['corechart', 'bar', 'timeline', 'calendar'] });

  setUserProfile();

  const root = null;
  const useHash = false;
  const router = new Navigo(root, useHash);

  router
    .on('/flows', function() {
      charts();
    })
    .on('/log', function() {
      logs();
    }).on(function() {
      charts();
    }).resolve();

});
