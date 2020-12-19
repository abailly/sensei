import style from './style.css';
import charts from './charts';
import logs from './logs';
import notes from './notes';
import { setUserProfile } from './user.js';
import { formatISODate } from './date.js';
import Navigo from 'navigo';

document.addEventListener('DOMContentLoaded', () => {
  google.charts.load('current', { 'packages': ['corechart', 'bar', 'timeline', 'calendar'] });

  setUserProfile();

  const root = null;
  const useHash = true;
  const router = new Navigo(root, useHash);

  router
    .on('/flows', function() {
      charts();
    })
    .on('/notes', function() {
      notes(router, document.getElementById('main'), formatISODate(new Date()));
    })
    .on('/notes/:page', function(params) {
      notes(router, document.getElementById('main'), params.page);
    })
    .on('/log/:page', function(params) {
      logs(router, document.getElementById('main'), params.page);
    })
    .on('/log', function() {
      logs(router, document.getElementById('main'), 1);
    })
    .on(function() {
      charts();
    }).resolve();

});
