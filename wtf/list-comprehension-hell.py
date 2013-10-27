msg = "Ett meddelande med en spotify:track:2eUomofxqu973hHJJuIj51 Spotify URI."
spotify_url_re = re.compile(r'spotify:([a-z]+?):(.+?)( (.*)|$)')


matches = set(spotify_url_re.findall(msg))
titles = [re.sub(r'(.+?) by (.+?) on Spotify', r'Spotify: \1 (\2)', title)
          for title in [get_title("http://open.spotify.com/{0}/{1}".format(type, id))
                        for (type, id) in  [(m[0], m[1]) for m in matches]]]