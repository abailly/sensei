import { dom, clear, clearElement } from './dom';
import { config } from './config';
import { get } from './request';
import *  as vis from 'vis-network';


function drawGoalsGraph(container, { goalsGraph }) {
  console.log(goalsGraph);
  // create an array with nodes
  var nodesMap = {};
  const nodes = goalsGraph.map((adj, index) => {
    nodesMap[adj[0]] = index;
    return { id: index, label: adj[0] };
  });

  var edges = [];

  goalsGraph.forEach((adj, index) =>
    adj[1].forEach((tgt) => {
      const idx = nodesMap[tgt];
      edges.push({ from: index, to: idx });
    }));

  var data = {
    nodes,
    edges,
  };
  var options = {
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
