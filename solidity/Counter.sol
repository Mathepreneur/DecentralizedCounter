pragma solidity ^0.6.4;

contract Counter {
    // MODEL
    uint256 public counter;
    // INIT
    constructor() public {
        counter = 0;
    }
    // UPDATE
    event Increment(uint256 counter);
    event Decrement(uint256 counter);
    function increment() external {
        require(counter != uint256(-1), "Out of bounds.");
        counter ++;
        emit Increment(counter);
    }
    function decrement() external {
        require(counter != 0, "Out of bounds.");
        counter --;
        emit Decrement(counter);
    }
}