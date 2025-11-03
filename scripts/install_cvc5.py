import platform
from urllib.request import urlretrieve
import os

def download():
    match platform.system().lower():
        case 'linux':
            system = 'Linux'
        case 'darwin':
            system = 'macOS'
        case other:
            raise ValueError(f"Unsupported platform '{other}'")
        
    match platform.machine().lower():
        case 'x86_64' | 'amd64':
            machine = 'x86_64'
        case 'arm64' | 'aarch64':
            machine = 'arm64'
        case other:
            raise ValueError(f"Unsupported machine '{other}'")

    url = f'https://github.com/cvc5/cvc5/releases/download/cvc5-1.3.1/cvc5-{system}-{machine}-java-api.jar'
    print(f'Downloading CVC5 Java API: {url}')
    urlretrieve(url, 'lib/cvc5.jar')

if __name__ == "__main__":
    files = os.listdir('.')
    if not ('scripts' in files and 'install_cvc5.py' in os.listdir('scripts')):
        raise RuntimeError("This script must be run from the project root directory.")
    os.makedirs('lib', exist_ok=True)
    download()