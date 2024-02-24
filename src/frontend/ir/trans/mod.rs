use super::Ir;

pub mod symbol_resolution;

pub struct IrTransforamtion {}
impl IrTransforamtion {
    pub fn transform(mut ir: Ir) -> Ir {
        Self::transform_mut(&mut ir);
        ir
    }
    pub fn transform_mut(ir: &mut Ir) {
        symbol_resolution::resolve_symbols_mut(ir);
    }
}
