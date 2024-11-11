import parse from 'parse-link-header';

/** Extract `Link` header(s) data from the given request.
The request is expected to be completed, eg. this should be called when
it's fully loaded. If there is a  `Link` header, its content will be parsed
and an object returned, otherwise it returns `null`.

@param {XMLHttpRequest} xhr The fully loaded request object from which
  to extract response headrer
@see https://www.npmjs.com/package/parse-link-header for
*/
function extractLinks(xhr) {
  const linkHdr = xhr.getResponseHeader('Link');
  if (linkHdr) {
    return parse(linkHdr);
  } else {
    return null;
  }
}

/** Make a GET query on server expecting JSON content */
export function get(router, url, callback, reject) {
  const xhr = new XMLHttpRequest();
  xhr.open('GET', url);
  xhr.setRequestHeader('X-API-Version', VERSION);
  xhr.onload = function() {
    if (xhr.status >= 200 && xhr.status < 300) {
      try {
        const links = extractLinks(xhr);
        // return an empty object when result is NoContent
        const flowData = xhr.status == 204 ? {} : JSON.parse(xhr.responseText);
        if (links) {
          callback(flowData, links);
        } else {
          callback(flowData);
        }
      } catch (e) {
        // JSON.parse can throw a SyntaxError
        if (e instanceof SyntaxError) {
          reject("invalid JSON payload" + xhr.responseText);
        } else {
          throw e;
        }
      }
    } else if (xhr.status == 401) {
      router.navigate('/login');
    } else {
      reject('Request failed.  Returned status of ' + xhr.status);
    }
  };
  xhr.send();
}


export function post(router, url, data, callback) {
  const xhr = new XMLHttpRequest();
  xhr.open('POST', url);
  xhr.setRequestHeader('X-API-Version', VERSION);
  xhr.setRequestHeader('Content-type', 'application/json');
  xhr.onload = function() {
    if (xhr.status >= 200 && xhr.status < 300) {
      try {
        if (xhr.responseText) {
          const flowData = JSON.parse(xhr.responseText);
          const links = extractLinks(xhr);
          if (links) {
            callback(flowData, links);
          } else {
            callback(flowData);
          }
        } else {
          callback();
        }
      } catch (e) {
        // JSON.parse can throw a SyntaxError
        if (e instanceof SyntaxError) {
          alert("invalid JSON payload" + xhr.responseText);
        }
        throw e;
      }
    } else if (xhr.status == 401) {
      router.navigate('/login');
    } else {
      alert('Request failed.  Returned status of ' + xhr.status);
    }
  };
  xhr.send(JSON.stringify(data));
}
