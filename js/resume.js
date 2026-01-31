(function($) {
  "use strict"; // Start of use strict

  function getMobileScrollOffset() {
    // On mobile, the navbar is fixed-top and covers anchor targets.
    // On desktop (>= 992px), the navbar is a fixed sidebar, so no offset is needed.
    if (window.matchMedia && window.matchMedia('(min-width: 992px)').matches) {
      return 0;
    }
    var $sideNav = $('#sideNav');
    return ($sideNav && $sideNav.length) ? ($sideNav.outerHeight() || 0) : 0;
  }

  function prefersReducedMotion() {
    return window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  }

  // Smooth scrolling using jQuery easing
  $('a.js-scroll-trigger[href*="#"]:not([href="#"])').click(function() {
    if (location.pathname.replace(/^\//, '') == this.pathname.replace(/^\//, '') && location.hostname == this.hostname) {
      var target = $(this.hash);
      target = target.length ? target : $('[name=' + this.hash.slice(1) + ']');
      if (target.length) {
        var top = target.offset().top - getMobileScrollOffset();
        if (top < 0) top = 0;

        if (prefersReducedMotion()) {
          window.scrollTo(0, top);
        } else {
          $('html, body').animate({
            scrollTop: top
          }, 800, "easeInOutExpo");
        }
        return false;
      }
    }
  });

  // Closes responsive menu when a scroll trigger link is clicked
  $('.js-scroll-trigger').click(function() {
    $('.navbar-collapse').collapse('hide');
  });

  // Activate scrollspy to add active class to navbar items on scroll
  $('body').scrollspy({
    target: '#sideNav',
    offset: getMobileScrollOffset() + 20
  });

  $(window).on('resize', function() {
    $('body').scrollspy('refresh');
  });

})(jQuery); // End of use strict
