import operator


class Comparable:
    ''' Mixin from regebro's blog from 2010.

    Uses `self._cmpkey()` as comparison key.
    '''

    def _compare(self, other, op):
        try:
            return op(self._cmpkey(), other._cmpkey())
        except (AttributeError, TypeError):
            raise NotImplemented

    def __eq__(self, other):
        return self._compare(other, operator.eq)

    def __ne__(self, other):
        return self._compare(other, operator.ne)

    def __lt__(self, other):
        return self._compare(other, operator.lt)

    def __le__(self, other):
        return self._compare(other, operator.le)

    def __gt__(self, other):
        return self._compare(other, operator.gt)

    def __ge__(self, other):
        return self._compare(other, operator.ge)
