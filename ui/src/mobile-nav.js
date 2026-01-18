/**
 * @fileOverview Mobile navigation hamburger menu functionality
 * @name mobile-nav.js
 */

/**
 * Initialize the hamburger menu toggle functionality
 */
export function initMobileNav() {
  const hamburger = document.getElementById('hamburger');
  const navlist = document.getElementById('navlist');

  if (!hamburger || !navlist) return;

  hamburger.addEventListener('click', () => {
    hamburger.classList.toggle('active');
    navlist.classList.toggle('nav-open');
  });

  // Close menu when a link is clicked
  navlist.addEventListener('click', (e) => {
    if (e.target.tagName === 'A') {
      hamburger.classList.remove('active');
      navlist.classList.remove('nav-open');
    }
  });

  // Close menu when clicking outside
  document.addEventListener('click', (e) => {
    if (!hamburger.contains(e.target) && !navlist.contains(e.target)) {
      hamburger.classList.remove('active');
      navlist.classList.remove('nav-open');
    }
  });
}
