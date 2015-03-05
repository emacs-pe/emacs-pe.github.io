// Stolen from: https://github.com/h5bp/h5bp.github.io/blob/master/assets/app.js

(function ($, undefined) {
  'use strict';

  var orgName = 'emacs-pe';

  function getRepoUrl(repo) {
    return repo.homepage || repo.html_url;
  }

  function getRepoDesc(repo) {
    return repo.description;
  }

  function showRepo(repo) {
    var $item = $('<div class="unit-1-3 repo" />');
    var $link = $('<a class="box" href="' + getRepoUrl(repo) + '" />');

    $link.append('<h3 class="repo__name">' + repo.name + '</h3>');
    $link.appendTo($item);

    $item.append('<p class="repo__info">' + repo.watchers + ' stargazers ' + (repo.language !== null ? '&middot; ' + repo.language : '') + '</p>');
    $item.append('<p class="repo__desc">' + getRepoDesc(repo) + '</p>');
    $item.appendTo('#projects');
  }

  $.getJSON('https://api.github.com/orgs/' + orgName + '/repos?callback=?', function (result) {
    var repos = result.data;
    $(function () {
      $('#num-repos').text(repos.length);

      // Convert pushed_at to Date.
      $.each(repos, function (i, repo) {
        repo.pushed_at = new Date(repo.pushed_at);

        var weekHalfLife  = 1.146 * Math.pow(10, -9);

        var pushDelta    = (new Date()) - Date.parse(repo.pushed_at);
        var createdDelta = (new Date()) - Date.parse(repo.created_at);

        var weightForPush = 1;
        var weightForWatchers = 1.314 * Math.pow(10, 7);

        repo.hotness = weightForPush * Math.pow(Math.E, -1 * weekHalfLife * pushDelta);
        repo.hotness += weightForWatchers * repo.watchers / createdDelta;
      });

      // Sort by hotness.
      repos.sort(function (a, b) {
        if (a.hotness < b.hotness) return 1;
        if (b.hotness < a.hotness) return -1;
        return 0;
      });

      $.each(repos, function (i, repo) {
        showRepo(repo);
      });

      // Sort by most-recently pushed to.
      repos.sort(function (a, b) {
        if (a.pushed_at < b.pushed_at) return 1;
        if (b.pushed_at < a.pushed_at) return -1;
        return 0;
      });
    });
  });
})(jQuery);
