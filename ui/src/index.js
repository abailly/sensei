import style from './style.css';
import charts from './charts';
import logs from './logs';
import notes from './notes';
import { login } from './auth';
import { summaries } from './summaries';
import { goals } from './goals';
import { setUserProfile } from './user.js';
import { formatISODate } from './date.js';
import Navigo from 'navigo';
import { LocalDate } from "@js-joda/core";

document.addEventListener('DOMContentLoaded', () => {
  google.charts.load('current', { 'packages': ['corechart', 'bar', 'timeline', 'calendar'] });

  const root = null;
  const useHash = true;
  const router = new Navigo(root, useHash);

  // default date used to display flows and notes
  const today = formatISODate(LocalDate.now());

  setUserProfile(router);

  router
    .on('/flows', function() {
      router.navigate('/flows/' + today);
    })
    .on('/flows/:date', function(params) {
      charts(router, params.date);
    })
    .on('/login', function() {
      login(router, document.getElementById('main'), document.getElementById('navlist'));
    })
    .on('/notes', function() {
      notes.list(router, document.getElementById('main'), today);
    })
    .on('/notes/:page', function(params) {
      notes.list(router, document.getElementById('main'), params.page);
    })
    .on('/search', function() {
      notes.search(router, document.getElementById('main'));
    })
    .on('/log/:page', function(params) {
      logs(router, document.getElementById('main'), params.page);
    })
    .on('/log', function() {
      logs(router, document.getElementById('main'), 1);
    })
    .on('/summaries/:from/:to/:period', function(params) {
      summaries(router, document.getElementById('main'), params.from, params.to, params.period);
    })
    .on('/summaries', function() {
      summaries(router, document.getElementById('main'));
    })
    .on('/goals', function() {
      goals(router, document.getElementById('main'));
    })
    .on(function() {
      // default to flows for today
      router.navigate('/flows/' + today);
    }).resolve();

});
