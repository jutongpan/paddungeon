from flask import Flask, render_template, request, jsonify
import sqlite3
import pandas as pd
import re
import urllib.request

class ReverseProxied(object):
    def __init__(self, app):
        self.app = app

    def __call__(self, environ, start_response):
        script_name = environ.get('HTTP_X_SCRIPT_NAME', '')
        if script_name:
            environ['SCRIPT_NAME'] = script_name
            path_info = environ['PATH_INFO']
            if path_info.startswith(script_name):
                environ['PATH_INFO'] = path_info[len(script_name):]

        scheme = environ.get('HTTP_X_SCHEME', '')
        if scheme:
            environ['wsgi.url_scheme'] = scheme
        return self.app(environ, start_response)

app = Flask(__name__)
app.wsgi_app = ReverseProxied(app.wsgi_app)

df_dungeon = pd.read_csv("dungeon.csv")

@app.route('/')
def index():
    return render_template(
        'index.html',
        df_dungeon = df_dungeon
        )

@app.route('/<dungeon>', methods=['GET', 'POST'])
def dungeonPage(dungeon):
    if request.method == 'POST':
        return redirect(url_for('index'))

    return render_template(
        'dungeonPage.html',
        dungeonSeries = df_dungeon.loc[df_dungeon.dungeonName == dungeon].dungeonSeries.tolist()[0],
        dungeonName = dungeon,
        dungeonHtml = 'dungeonHtml/' + dungeon + '.html'
        )

if __name__ == '__main__':
    app.run(debug=True)
