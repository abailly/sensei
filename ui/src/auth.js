import { dom, clear, clearElement } from './dom';
import { post } from './request';
import { config } from './config.js';

export function login(router, container) {
  const content = <div id='login'>
    <div class='field'><label for='username'>Login</label><input name='username' id='username' type='text' /></div>
    <div class='field'><label for='password'>Password</label><input name='password' id='password' type='password' /></div>
    <div class='control'><button onclick={doLogin}>Login</button></div>
  </div>;

  function doLogin() {
    const credLogin = document.getElementById('username').value;
    const credPassword = document.getElementById('password').value;
    post(router, '/login', { credLogin, credPassword }, (profile) => { config.userProfile = profile; router.navigate('/'); });
  };

  clearElement(container);
  container.appendChild(content);
};
