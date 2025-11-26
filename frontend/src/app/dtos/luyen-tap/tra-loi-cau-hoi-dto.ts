// src/app/dtos/practice/tra-loi-cau-hoi-dto.ts
export interface CauTraLoiPracticeDTO {
  cau_hoi_id: number;
  lua_chon: 'A' | 'B' | 'C' | 'D' | null;
  thoi_gian_ms?: number | null;
}

export interface TraLoiCauHoiPracticeDTO {
  phien_id: number;
  cau_tra_loi_list: CauTraLoiPracticeDTO[];
}
