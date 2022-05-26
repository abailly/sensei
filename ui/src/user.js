import { get } from './request.js';

export function setUserProfile(router) {
  return new Promise((resolve, reject) =>
    get(router,
      '/api/users',
      (userProfile) => resolve(userProfile),
      reject));
}
