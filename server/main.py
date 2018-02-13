from flask import Flask
app = Flask(__name__)


@app.route("/")
def main():
    return "Hey Main!"


@app.route("/first")
def hello():
    return "Hey First!"


@app.route("/second")
def inny():
    return "Hey Second!"


if __name__ == "__main__": 
    app.run(host="0.0.0.0", port=80)