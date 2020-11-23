import { get } from './request.js';
import { config } from './config.js';

export function setUserProfile() {
  get(`/users/${config.user}`, (userProfile) =>
    config.userProfile = userProfile
  );
}
