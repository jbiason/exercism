"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const hello_world_1 = require("./hello-world");
describe('Hello World', () => {
    it('says hello world', () => {
        expect(hello_world_1.hello()).toEqual('Hello, World!');
    });
});
