/**
 * @fileOverview
 * @name dom.js
 * @author arnaud@pankzsoft.com
 * @license MIT
 */



/**
 * Basic implementation of JSX element transformation function.
 * Stolen from <a href='https://blog.r0b.io/post/using-jsx-without-react/'>this blog post</a>.
 *
 * @param {String} tagName the name of the tag to create
 * @param {Object} attrs an object defining all the attributes of the new node
 * @param {any} children a possibly empty sequence of children to add to this node.
 * @returns {Object} a newly initialized DOM element with given attributes and children
 */
export function dom(tagName, attrs = {}, ...children) {
  const elem = Object.assign(document.createElement(tagName), attrs);
  for (const child of children) {
    if (Array.isArray(child)) elem.append(...child);
    else elem.append(child);
  }
  return elem;
}

export function clear(parentId) {
  const timelines = document.getElementById(parentId);
  clearElement(timelines);
}

export function clearElement(elem) {
  while (elem.firstChild) {
    elem.removeChild(elem.firstChild);
  }
}
