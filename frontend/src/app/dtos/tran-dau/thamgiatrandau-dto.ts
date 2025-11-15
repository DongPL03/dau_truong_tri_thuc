export class ThamGiaTranDauDTO {
  tran_dau_id: number;
  ma_pin?: string | null;

  constructor(data: Partial<ThamGiaTranDauDTO>) {
    this.tran_dau_id = data.tran_dau_id ?? 0;
    this.ma_pin = data.ma_pin ?? null;
  }
}
