{
  # Set firefox as the default browser
  home.sessionVariables = { BROWSER = "firefox"; };

  programs.firefox = {
    enable = true;

    # Setup firefox (settings are from the about:config page in firefox)
    profiles.default.settings = {
      # Do not show the warning when opening the `about:config` page
      "browser.aboutConfig.showWarning" = false;
      # Do not automatically fill in forms
      "browser.formfill.enable" = false;
      # Do not check if Firefox is the default browser
      "browser.shell.checkDefaultBrowser" = false;
      # Start Firefox with an empty page
      "browser.startup.homepage" = "about:blank";
      # Disable the Firefox View feature
      "browser.tabs.firefox-view" = false;
      # Set the default text in the address bar to "Search with DuckDuckGo or enter address"
      "browser.urlbar.placeholderName" = "DuckDuckGo";
      # Same as the line above, but for private tabs
      "browser.urlbar.placeholderName.private" = "DuckDuckGo";
      # Do not autoplay audio and video content on websites
      "media.autoplay.default" = 5;
      # Prevent websites from requesting permission to send desktop notifications
      "permissions.default.desktop-notification" = 2;
    };
  };
}
