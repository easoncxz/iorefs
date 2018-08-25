from abc import abstractmethod

from typing import TypeVar, Any
from typing_extensions import Protocol

T = TypeVar('T', contravariant=True, bound='Comparable')


class Comparable(Protocol):
    ''' Not sure what I'm doing here

    I seem to want something like `Comparable(Protocol[T])` where
    `T = TypeVar('T', contravariant=True)` but it's difficult with
    forward references of type variables not allowed on the usage site.
    '''

    @abstractmethod
    def __eq__(self: T, other: Any) -> bool:
        pass

    def __ne__(self: T, other: Any) -> bool:
        return not (self == other)

    @abstractmethod
    def __lt__(self: T, other: T) -> bool:
        pass

    def __gt__(self: T, other: T) -> bool:
        return not (self < other) and self != other

    def __le__(self: T, other: T) -> bool:
        return self < other or self == other

    def __ge__(self: T, other: T) -> bool:
        return not (self < other)
