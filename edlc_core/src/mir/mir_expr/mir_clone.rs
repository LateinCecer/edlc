use crate::file::ModuleSrc;
use crate::lexer::SrcPos;
use crate::mir::mir_expr::MirTempVar;
use crate::mir::mir_type::MirTypeId;
use crate::mir::MirUid;

pub struct MirClone {
    pub val: MirTempVar,
    pub pos: SrcPos,
    pub src: ModuleSrc,
    pub uid: MirUid,
    pub ty: MirTypeId,
}
