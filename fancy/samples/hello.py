#!/usr/bin/env python3
"""sample python file"""
def greet(name: str) -> str:
    # TODO: i18n
    return f"hello, {name}"

if __name__ == "__main__":
    print(greet("world"))
