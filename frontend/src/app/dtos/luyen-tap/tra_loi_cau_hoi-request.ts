export interface CauTraLoiRequest {
  cau_hoi_id: number;
  lua_chon: string;      // 'A' | 'B' | 'C' | 'D'
  thoi_gian_ms?: number; // optional
}

export interface TraLoiCauHoiRequest {
  phien_id: number;
  cau_tra_loi_list: CauTraLoiRequest[];
}
