export class SubmitAnswerDTO {
  tran_dau_id: number;
  cau_hoi_id: number;
  answer: string; // A/B/C/D

  constructor(tran_dau_id: number, cau_hoi_id: number, answer: string) {
    this.tran_dau_id = tran_dau_id;
    this.cau_hoi_id = cau_hoi_id;
    this.answer = answer;
  }
}
