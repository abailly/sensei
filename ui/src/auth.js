import { dom, clear, clearElement } from './dom';
import { post, get } from './request';
import { config } from './config.js';


/** Build 'login' component and attaches it to given `container`.

When login is successful, it adds a logout button to the given `navItemsList`.

@param {Router} router Upon successful login it navigates to root page
@param {DOM} container Where to attach the login component to
@param {DOM} navItemsList Where to add a logout button
*/
export function login(router, container, navItemsList) {
  const content = <div id='login'>
    <div class='field'><label for='username'>Login</label><input name='username' id='username' type='text' /></div>
    <div class='field'><label for='password'>Password</label><input name='password' id='password' type='password' /></div>
    <div class='control'><button id='dologin' onclick={doLogin}>Login</button></div>
  </div>;

  function doLogin() {
    const credLogin = document.getElementById('username').value;
    const credPassword = document.getElementById('password').value;
    post(router, '/login', { credLogin, credPassword }, (profile) => {
      logout(router, navItemsList);
      config.userProfile = profile;
      router.navigate('/');
    });
  };

  clearElement(container);
  container.appendChild(content);
};

function logout(router, container) {
  const content = <li id='logout'>
    <a onclick={doLogout}>Logout</a>
  </li>;

  function doLogout() {
    container.removeChild(content);
    get(router, '/logout', () => {
      config.userProfile = null;
      router.navigate('/login');
    });
  };

  container.appendChild(content);
}
