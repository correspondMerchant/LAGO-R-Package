"""R's NA in a converted result becomes a missing value, not a sentinel."""
from lago import _bridge as b


def test_r_na_in_integer_logical_and_text_becomes_missing(ro):
    with b.r_context():
        df = b.r_to_py(ro.r("data.frame(a = c(TRUE, NA), i = c(1L, NA), s = c('x', NA))"))
        assert df["a"].isna().tolist() == [False, True]
        assert bool(df["a"].tolist()[0]) is True
        assert df["i"].isna().tolist() == [False, True]
        assert int(df["i"].tolist()[0]) == 1
        # a column with no NA keeps its plain type
        whole = b.r_to_py(ro.r("data.frame(i = 1:2)"))
        assert whole["i"].tolist() == [1, 2]
        assert b.r_to_py(ro.r("c(1L, NA)")) == [1, None]
        assert b.r_to_py(ro.r("c(TRUE, NA)")) == [True, None]
        assert b.r_to_py(ro.r("c('x', NA)")) == ["x", None]
