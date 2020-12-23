import { dom, clearElement } from './dom';
import { config } from "./config";
import { colorOf } from './color.js';
import { get } from './request';
import { pagination } from './page.js';

/*
  Display (latest) event log entries from the server
*/
export default function logs(router, container, page) {
  clearElement(container);
  get(`/log/${config.user}?page=${page}`, (logEntries, links) => {
    const logDiv = pagination('log', router, links);
    const logTable =
      <table class='tbl-log'>
        <thead>
          <tr><th>Timestamp</th><th>Type</th><th>Data</th></tr>
        </thead>
        <tbody>
          {
            logEntries.map(entry => {
              const flowType = entry.flowType ?? entry.tag;
              const color = colorOf(flowType);
              return <tr class={flowType} style={`background-color: ${color};`} >
                <td>{entry.traceTimestamp ?? entry.flowTimestamp ?? entry.noteTimestamp}</td>
                <td>{flowType}</td>
                <td>{JSON.stringify(entry)}</td>
              </tr>;
            })
          }
        </tbody>
      </table>;
    container.appendChild(logDiv);
    container.appendChild(logTable);
  });
};
