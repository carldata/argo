from flask import Flask
app = Flask(__name__)

@app.route("/first")
def hello():
    return "Hey First!"

@app.route("/second")
def inny():
    return "Hey Second!"

if __name__ == "__main__":
    app.run(debug=True, host='0.0.0.0')