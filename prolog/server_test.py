from multiprocessing import Process
import subprocess
import time

""" Utility script for launching server via main.pl and client via server_test.pl in separate
processes to facilitate end-to-end test."""

def launch_server():
    """Launches the server via main.pl on port 5000, waits 5 seconds, and then closes it."""
    with subprocess.Popen(
        # swipl main.pl launch 5000 - as in README.md
        ["swipl", "main.pl", "launch", "5000"],
        # Intercept pipes.
        stdout=subprocess.PIPE,
        stdin=subprocess.PIPE,
        stderr=subprocess.PIPE
    ) as proc:
        # Sleep, then send quit to main.pl to make it exit.
        time.sleep(5)
        proc.stdin.write(bytes("quit\n", "utf8"))


if __name__ == "__main__":
    # Launches server in background process
    p = Process(target=launch_server)
    p.start()
    # Give server time to come online
    time.sleep(2)
    # Run unit tests in server_test.pl
    process = subprocess.run(
        ["swipl", "-g", "run_tests", "-t", "halt", "server_test.pl"],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    # Print all output from unit test
    print(process.stdout.decode("utf8"))
    # Bubble up return code.
    process.check_returncode()
    # Join server process.
    p.join()
