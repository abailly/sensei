import { get } from './request.js';
import { config } from './config.js';

export function setUserProfile(router) {
  get(router, `/api/users/${config.user}`, (userProfile) =>
    config.userProfile = userProfile
  );
}
