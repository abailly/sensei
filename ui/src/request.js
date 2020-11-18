export function get(url, callback) {
  const xhr = new XMLHttpRequest();
  xhr.open('GET', url);
  xhr.setRequestHeader('X-API-Version', VERSION);
  xhr.onload = function() {
    if (xhr.status >= 200 && xhr.status < 300) {
      try {
        const flowData = JSON.parse(xhr.responseText);
        callback(flowData);
      } catch (e) {
        // JSON.parse can throw a SyntaxError
        if (e instanceof SyntaxError) {
          alert("invalid JSON payload" + xhr.responseText);
        }
        throw e;
      }
    } else {
      alert('Request failed.  Returned status of ' + xhr.status);
    }
  };
  xhr.send();
}
