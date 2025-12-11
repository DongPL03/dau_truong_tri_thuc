export class SendMessageDto {
  receiver_id!: number;
  noi_dung!: string;

  constructor(data?: Partial<SendMessageDto>) {
    if (data) {
      Object.assign(this, data);
    }
  }
}
