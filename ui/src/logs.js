import { dom, clearElement } from './dom';
import { config } from "./config";
import { colorOf } from './color.js';
import { get } from './request';
import { pagination } from './page.js';

function formatTraceEntry(entry) {
  return <tr class='log-entry entry-trace'>
    <td>{entry.index}</td>
    <td>{entry.event.traceTimestamp}</td>
    <td>Trace</td>
    <td>{entry.event.traceDirectory}</td>
    <td>{JSON.stringify(entry.event)}</td>
  </tr>;
}

function formatNoteEntry(entry) {
  return <tr class='log-entry entry-note'>
    <td>{entry.index}</td>
    <td>{entry.event.noteTimestamp}</td>
    <td>Note</td>
    <td>{entry.event.noteDir}</td>
    <td>{entry.event.noteContent}</td>
  </tr>;
}

function formatFlowEntry(entry) {
  const color = colorOf(entry.flowType);
  return <tr class='log-entry entry-flow' style={`background-color: ${color};`} >
    <td>{entry.index}</td>
    <td>{entry.event.flowTimestamp}</td>
    <td>Flow</td>
    <td>{entry.event.flowDir}</td>
    <td>{entry.event.flowType}</td>
  </tr>;
}

function formatArticleEntry(entry) {
  switch(entry.event.operation) {
    case 'PublishArticle':
      return <tr class='log-entry entry-note'>
        <td>{entry.index}</td>
        <td>{entry.event.articleTimestamp}</td>
        <td>Publish Article</td>
        <td>{entry.event.articleDir}</td>
        <td>{entry.event.article.substring(0, 100)}...</td>
        </tr>;
    case 'UpdateArticle':
      return <tr class='log-entry entry-note'>
        <td>{entry.index}</td>
        <td>{entry.event.articleTimestamp}</td>
        <td>Update Article ({entry.event.articleRkey})</td>
        <td>{entry.event.articleDir}</td>
        <td>{entry.event.article.substring(0, 100)}...</td>
        </tr>;
    case 'DeleteArticle':
      return <tr class='log-entry entry-note'>
        <td>{entry.index}</td>
        <td>{entry.event.articleTimestamp}</td>
        <td>Delete Article ({entry.event.articleRkey})</td>
        <td>{entry.event.articleDir}</td>
        <td></td>
      </tr>;
    default:
     throw new Error(`Unknown article operation: ${entry.event.operation}`);
  }
}


function formatLogEntry(entry) {
  switch (entry.event.tag) {
    case 'Trace': return formatTraceEntry(entry);
    case 'Note': return formatNoteEntry(entry);
    case 'Flow': return formatFlowEntry(entry);
    case 'Article': return formatArticleEntry(entry);
    default: return '';
  };
}

/*
  Display (latest) event log entries from the server
*/
export default function logs(router, container, page) {
  clearElement(container);
  get(router, `/api/log/${config.userProfile.userName}?page=${page}`, (logEntries, links) => {
    const logDiv = pagination('log', router, links);
    const logTable =
      <table class='tbl-log'>
        <thead>
          <tr><th>Index</th><th>Timestamp</th><th>Type</th><th>Dir</th><th>Details</th></tr>
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
