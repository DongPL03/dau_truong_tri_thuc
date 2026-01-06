export type LoaiTinNhanChat =
  | 'VAN_BAN'
  | 'HINH_ANH'
  | 'TAP_TIN'
  | 'AM_THANH'
  | 'HE_THONG'
  | 'STICKER'
  | 'EMOJI';

export class SendMessageDto {
  receiver_id!: number;
  noi_dung?: string;
  loai_tin_nhan?: LoaiTinNhanChat;
  url_media?: string;
  ten_file?: string;
  kich_thuoc_file?: number;

  constructor(data?: Partial<SendMessageDto>) {
    if (data) {
      Object.assign(this, data);
    }
  }
}
