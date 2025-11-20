import {Component, OnInit, ViewChild} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule, NgForm} from '@angular/forms';
import Swal from 'sweetalert2';
import {Base} from '../../base/base';
import {ResponseObject} from '../../../responses/response-object';
import {CauHoiResponse} from '../../../responses/cauhoi/cauhoi-response';
import {CauHoiDTO} from '../../../dtos/cau-hoi/cauhoi-dto';

@Component({
  selector: 'app-sua-bo-cau-hoi-question',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './sua-cau-hoi.html',
  styleUrl: './sua-cau-hoi.scss'
})
export class CauHoiEdit extends Base implements OnInit {
  @ViewChild('form') form!: NgForm;
  questionId!: number;
  boCauHoiId!: number;
  question?: CauHoiResponse | null;
  loading = true;
  saving = false;
  previewUrl?: string;
  readonly imageBaseUrl = 'http://localhost:8088/api/v1/cauHoi/media/';

  model: CauHoiDTO = new CauHoiDTO();

  ngOnInit(): void {
    this.boCauHoiId = Number(this.route.snapshot.paramMap.get('boId'));
    this.questionId = Number(this.route.snapshot.paramMap.get('id'));
    this.loadQuestion();
  }

  loadQuestion() {
    this.cauHoiService.getById(this.questionId).subscribe({
      next: (res: ResponseObject<CauHoiResponse>) => {
        this.question = res.data!;
        this.model = {...this.model, ...res.data};
        this.previewUrl = res.data?.duong_dan_tep
          ? `${this.imageBaseUrl}${res.data.duong_dan_tep.split('/').pop()}`
          : undefined;
        this.loading = false;
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải dữ liệu câu hỏi', 'error').then(r => {
        });
        this.loading = false;
      },
    });
  }

  onSubmit(form: NgForm) {
    if (form.invalid) return;

    this.saving = true;
    this.cauHoiService.update(this.questionId, this.model).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Cập nhật câu hỏi thành công', 'success').then(() =>
          this.router.navigateByUrl(`/bo-cau-hoi/chi-tiet-bo-cau-hoi/${this.boCauHoiId}`)
        );
      },
      error: () => Swal.fire('Lỗi', 'Cập nhật thất bại', 'error'),
      complete: () => (this.saving = false),
    });
  }

  onLoaiNoiDungChange() {
    // reset preview nếu đổi loại nội dung
    if (this.model.loai_noi_dung === 'VAN_BAN') this.previewUrl = undefined;
  }

  onFileSelected(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;
    const file = input.files[0];

    // preview tạm
    const reader = new FileReader();
    reader.onload = () => (this.previewUrl = reader.result as string);
    reader.readAsDataURL(file);

    // upload thực tế
    this.cauHoiService.uploadMedia(this.questionId, file, this.model.loai_noi_dung).subscribe({
      next: (res) => {
        this.model.duong_dan_tep = res.data;
        Swal.fire('Thành công', 'Tải file thành công!', 'success').then(r => {
        });
      },
      error: () => Swal.fire('Lỗi', 'Không thể tải file', 'error'),
    });
  }

  backToBoCauHoiDetail() {
    this.router.navigateByUrl(`/bo-cau-hoi/chi-tiet-bo-cau-hoi/${this.boCauHoiId}`).then(r => {
    });
  }
}
