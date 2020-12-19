import { dom, clear } from './dom';
import { config } from "./config";
import { colorOf } from './color.js';
import { get } from './request';

function pagination(router, links) {
  if (!links) return "";

  return <div class='pagination'>
    {
      <button disabled={!links.prev} class='btn-prev' onclick={() => router.navigate(`/log/${links.prev.page}`)}>&lt;&lt;</button>
    }
    {
      <button disabled={!links.next} class='btn-next' onclick={() => router.navigate(`/log/${links.next.page}`)}>&gt;&gt;</button>
    }
  </div>;
}

/*
  Display (latest) event log entries from the server
*/
export default function logs(router, page) {
  clear('main');
  get(`/log/${config.user}?page=${page}`, (logEntries, links) => {
    const logDiv = pagination(router, links);
    const logTable =
      <table class='tbl-log'>
        <thead>
          <tr><th>Timestamp</th><th>Type</th><th>Data</th></tr>
        </thead>
        <tbody>
          {
            logEntries.map(entry => {
              const flowType = entry._flowType ?? "Trace";
              return <tr class={flowType}>
                <td>{entry.timestamp ?? entry._flowState._flowStart}</td>
                <td>{flowType}</td>
                <td>{JSON.stringify(entry._flowState ? entry._flowState : entry)}</td>
              </tr>;
            })
          }
        </tbody>
      </table>;
    document.getElementById('main').appendChild(logDiv);
    document.getElementById('main').appendChild(logTable);
  });
};
