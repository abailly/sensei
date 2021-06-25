import { dom, clearElement } from './dom';
import { config } from "./config";
import { colorOf } from './color.js';
import { get } from './request';
import { pagination } from './page.js';

function formatTraceEntry(entry) {
  return <tr class='log-entry entry-trace'>
    <td>{entry.traceTimestamp}</td>
    <td>Trace</td>
    <td>{entry.traceDirectory}</td>
    <td>{JSON.stringify(entry)}</td>
  </tr>;
}

function formatNoteEntry(entry) {
  return <tr class='log-entry entry-note'>
    <td>{entry.noteTimestamp}</td>
    <td>Note</td>
    <td>{entry.noteDir}</td>
    <td>{entry.noteContent}</td>
  </tr>;
}

function formatFlowEntry(entry) {
  const color = colorOf(entry.flowType);
  return <tr class='log-entry entry-flow' style={`background-color: ${color};`} >
    <td>{entry.flowTimestamp}</td>
    <td>Flow</td>
    <td>{entry.flowDir}</td>
    <td>{entry.flowType}</td>
  </tr>;
}

function formatLogEntry(entry) {
  switch (entry.tag) {
    case 'Trace': return formatTraceEntry(entry);
    case 'Note': return formatNoteEntry(entry);
    case 'Flow': return formatFlowEntry(entry);
    default: return '';
  };
}

/*
  Display (latest) event log entries from the server
*/
export default function logs(router, container, page) {
  clearElement(container);
  get(router, `/api/log/${config.user}?page=${page}`, (logEntries, links) => {
    const logDiv = pagination('log', router, links);
    const logTable =
      <table class='tbl-log'>
        <thead>
          <tr><th>Timestamp</th><th>Type</th><th>Dir</th><th>Details</th></tr>
        </thead>
        <tbody>
          {
            logEntries.map(formatLogEntry)
          }
        </tbody>
      </table>;
    container.appendChild(logDiv);
    container.appendChild(logTable);
  });
};
