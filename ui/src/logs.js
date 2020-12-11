import { dom, clear } from './dom';
import { config } from "./config";
import { colorOf } from './color.js';
import { get } from './request';

/*
  Display (latest) event log entries from the server
*/
export default function logs() {
  clear('main');
  get(`/log/${config.user}`, (logEntries) => {
    const logTable =
      <table>
        <thead>
          <tr><th>Timestamp</th><th>Type</th><th>Data</th></tr>
        </thead>
        <tbody>
          {
            logEntries.map(entry => {
              return <tr>
                <td>{entry.timestamp ?? entry._flowState._flowStart}</td>
                <td>{entry._flowType ?? "Trace"}</td>
                <td>{JSON.stringify(entry._flowState ? entry._flowState : entry)}</td>
              </tr>;
            })
          }
        </tbody>
      </table>;
    document.getElementById('main').appendChild(logTable);
  });
};
