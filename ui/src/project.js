import { dom, clearElement } from './dom.js';

export function drawProjectsSelector(selectorName, flows) {
  const projects = [];
  flows.forEach(f => {
    if (projects.indexOf(f.flowProject) == -1) {
      projects.push(f.flowProject);
    };
  });

  const selector = <select name={selectorName} id={selectorName}>
    <option value="all">All projects</option>
    {projects.map(p => (<option value={p}>{p}</option>))}
  </select>;

  return selector;
}
