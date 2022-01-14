import { dom, clear, clearElement } from './dom';
import { config } from './config';
import { get } from './request';
import *  as vis from 'vis-network';


export function computeGoalsGraph({ goalsGraph, current, completed }) {
  var nodesMap = {};
  const nodes = goalsGraph.map((adj, index) => {
    nodesMap[adj[0]] = index;
    var color = 'lightblue';
    if (current.indexOf(adj[0]) >= 0) {
      color = 'red';
    }
    if (completed.indexOf(adj[0]) >= 0) {
      color = 'green';
    }
    return { id: index, label: adj[0], color };
  });

  var edges = [];

  goalsGraph.forEach((adj, index) =>
    adj[1].forEach((tgt) => {
      const idx = nodesMap[tgt];
      edges.push({ from: index, to: idx });
    }));

  return {
    nodes,
    edges,
  };
}

function drawGoalsGraph(container, goals) {
  const data = computeGoalsGraph(goals);
  const options = {
    edges: {
      arrows: "to",
    },
    nodes: {
      shape: "dot",
      size: 10,
    },
  };
  return new vis.Network(container, data, options);
}

export function goals(router, container) {
  const content = <div class="goals">
  </div>;

  clearElement(container);
  container.appendChild(content);

  get(router, `/api/goals/${config.user}`, (goalsData) =>
    drawGoalsGraph(content, goalsData));

  return content;
}
