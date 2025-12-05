import {Component, OnInit, ViewChild} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule, NgForm} from '@angular/forms';
import Swal from 'sweetalert2';
import {Base} from '../../../base/base';
import {CauHoiResponse} from '../../../../responses/cauhoi/cauhoi-response';
import {CauHoiDTO} from '../../../../dtos/cau-hoi/cauhoi-dto';
import {ResponseObject} from '../../../../responses/response-object';


@Component({
  selector: 'app-admin-cau-hoi-edit',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './admin-cau-hoi-edit.html',
  styleUrl: './admin-cau-hoi-edit.scss'
})


export class AdminCauHoiEdit extends Base implements OnInit {
  @ViewChild('form') form!: NgForm;

  cau_hoi_id!: number;
  bo_cau_hoi_id!: number;

  question?: CauHoiResponse | null;
  model: CauHoiDTO = new CauHoiDTO();

  loading = true;
  saving = false;
  previewUrl?: string;

  readonly imageBaseUrl = 'http://localhost:8088/api/v1/cauHoi/media/';

  ngOnInit(): void {
    this.cau_hoi_id = Number(this.route.snapshot.paramMap.get('id'));
    this.loadQuestion();
  }

  loadQuestion(): void {
    this.cauHoiService.getById(this.cau_hoi_id).subscribe({
      next: (res: ResponseObject<CauHoiResponse>) => {
        this.question = res.data!;
        this.model = {...this.model, ...res.data};

        this.bo_cau_hoi_id = this.model.bo_cau_hoi_id;

        this.previewUrl = res.data?.duong_dan_tep
          ? `${this.imageBaseUrl}${res.data.duong_dan_tep.split('/').pop()!}`
          : undefined;

        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải dữ liệu câu hỏi', 'error')
          .then(() => this.router.navigate(['/admin/bo-cau-hoi']).then());
      }
    });
  }

  onSubmit(form: NgForm): void {
    if (form.invalid) return;

    this.saving = true;
    this.cauHoiService.update(this.cau_hoi_id, this.model).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Cập nhật câu hỏi thành công', 'success')
          .then(() => {
            this.router.navigate(['/admin/bo-cau-hoi', this.bo_cau_hoi_id]).then();
          });
      },
      error: () => {
        Swal.fire('Lỗi', 'Cập nhật thất bại', 'error');
      },
      complete: () => (this.saving = false)
    });
  }

  onLoaiNoiDungChange(): void {
    if (this.model.loai_noi_dung === 'VAN_BAN') {
      this.previewUrl = undefined;
    }
  }

  onFileSelected(event: Event): void {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const file = input.files[0];

    const reader = new FileReader();
    reader.onload = () => (this.previewUrl = reader.result as string);
    reader.readAsDataURL(file);

    this.cauHoiService.uploadMedia(this.cau_hoi_id, file, this.model.loai_noi_dung).subscribe({
      next: (res: ResponseObject<string>) => {
        this.model.duong_dan_tep = res.data;
        Swal.fire('Thành công', 'Tải file thành công!', 'success');
      },
      error: () => Swal.fire('Lỗi', 'Không thể tải file', 'error')
    });
  }

  backToBoCauHoiDetail(): void {
    this.router.navigate(['/admin/bo-cau-hoi', this.bo_cau_hoi_id]).then();
  }
}
