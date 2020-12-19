import { dom } from './dom';
import Navigo from 'navigo';


/** Builds a pagination element.
*
* @param {String} baseUri the base URI to use for routing pages. Page numbers will be appended
*  to this base URI.
* @param {Navigo} router the router object to use for navigating.
* @param {object} links an object containing links extracted from a Link header. See <a href="https://www.npmjs.com/package/parse-link-header">this package</a>
  for details.
*/
export function pagination(baseUri, router, links) {
  if (!links) return <span />;

  return <div class='pagination'>
    {
      <button disabled={!links.prev} class='btn-prev' onclick={() => router.navigate(`/${baseUri}/${links.prev.page}`)}>&lt;&lt;</button>
    }
    {
      <button disabled={!links.next} class='btn-next' onclick={() => router.navigate(`/${baseUri}/${links.next.page}`)}>&gt;&gt;</button>
    }
  </div>;
}
